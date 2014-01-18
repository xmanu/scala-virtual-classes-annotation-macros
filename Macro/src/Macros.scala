import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.collection.mutable.HashMap

object virtualContext {

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._

    def virtualTraitName(className: TypeName, enclClassName: TypeName) =
      "VC_TRAIT$" + enclClassName + "$" + className
    def factoryName(className: TypeName) =
      "VC_NEW$" + className
    def fixClassName(className: TypeName, enclClassName: TypeName) =
      "VC_FIX$" + enclClassName + "$" + className
    def finalClassName(className: TypeName) =
      "VC_FINAL$" + className

    val volatileFixMap: scala.collection.mutable.HashMap[String, String] = new scala.collection.mutable.HashMap()

    def noParameterConstructor = DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(()))))
    def noParameterTraitConstructor = DefDef(Modifiers(), newTermName("$init$"), List(), List(List()), TypeTree(), Block(List(), Literal(Constant(()))))

    def isVirtualClass(mods: c.universe.Modifiers) = {
      mods.annotations.foldRight(false)((a, b) => b || (a.toString == "new virtual()" || a.toString == "new virtualOverride()"))
    }

    def isOverridenVirtualClass(mods: c.universe.Modifiers) = {
      mods.annotations.foldRight(false)((a, b) => b || (a.toString == "new virtualOverride()"))
    }

    def parentIsVirtualClass(parent: Tree, virtualClass: TypeName) = {
      computeType(parent).members.exists(s => s.name.decoded.startsWith(virtualTraitName(virtualClass, getNameFromTree(parent))))
    }

    def typeCheckExpressionOfType(typeTree: Tree): Type = {
      val someValueOfTypeString = reify {
        def x[T](): T = throw new Exception
        x[String]()
      }

      val Expr(Block(stats, Apply(TypeApply(someValueFun, _), someTypeArgs))) = someValueOfTypeString

      val someValueOfGivenType = Block(stats, Apply(TypeApply(someValueFun, List(typeTree)), someTypeArgs))
      val someValueOfGivenTypeChecked = c.typeCheck(someValueOfGivenType)

      someValueOfGivenTypeChecked.tpe
    }

    def computeType(tpt: Tree): Type = {
      try {
        if (tpt.tpe != null) {
          tpt.tpe
        } else {
          val calculatedType = c.typeCheck(tpt.duplicate, silent = true, withMacrosDisabled = false).tpe
          val result = if (tpt.tpe == null) calculatedType else tpt.tpe

          if (result == NoType) {
            typeCheckExpressionOfType(tpt)
          } else {
            result
          }
        }
      } catch {
        case _: Throwable => NoType
      }
    }

    def getVolatileFixNameInParents(classNames: List[TypeName], enclName: TypeName, parent: TypeName): List[Name] = {
      classNames.flatMap(classNameLong =>
        try {
          val parentName = getParentNameFromSub(classNameLong.toString)
          //println("parentName: " + parentName)
          if (parentName.toString == enclName.toString)
            Nil
          else if (getNameFromSub(classNameLong.toString) == parent.toString) {
            val className = getNameFromSub(classNameLong.toString)
            val tpt = Ident(newTypeName(parentName))
            val tp = computeType(tpt)
            //println("getVolatileFixNameInParents: " + tp + " | " + virtualTraitName(className, parentName))
            //println(tp.declarations.mkString(" "))
            val traitTp = tp.declaration(newTypeName(virtualTraitName(className, parentName)))
            //println(traitTp)
            if (tp != NoType && traitTp != NoSymbol) {
              //List(newTypeName(virtualTraitName(className, parentName)))
              //println(traitTp.typeSignature)
              //println(traitTp.typeSignature.declarations.mkString(";"))
              val lst = traitTp.typeSignature.declarations.filter(d => d.name.toString.startsWith("volatileFix$")).map(_.name).toList
              //println(lst.mkString(" | "))
              lst
            } else
              Nil
          } else {
            Nil
          }
        } catch {
          case e: Throwable => e.printStackTrace(); Nil
        })
    }

    def getVolatileFixName(parentName: TypeName, familyBody: List[Tree], enclName: TypeName) = {
      //println("getVolatileFixName: " + parentName.toString + "; " + enclName.toString + "; " + familyBody.mkString("|"))
      val possibleNames: List[Name] = familyBody.flatMap(b => b match {
        case ClassDef(mods, name, tparams, Template(parents, self, body)) if (name.toString == parentName.toString) =>
          //println("found some class")
          List(newTermName("volatileFix$" + name))
        case _ => List()
      }) //++ getVolatileFixNameInParents(parentName, enclName)

      //println(possibleNames.mkString(" "))

      if (possibleNames.length > 0)
        Some(possibleNames(0))
      else
        None
    }

    def transformVCBody(body: List[Tree], parents: List[Tree], bodies: List[Tree], name: TypeName, mods: Modifiers, parentName: TypeName, enclName: TypeName, inheritRel: List[String]) = {
      val constructorTransformed = body.map(d => d match {
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) if (name.toString == "<init>") =>
          noParameterTraitConstructor
        case _ => d
      })

      val volatileFixesIntro: List[Tree] = {
        if (mods.hasFlag(ABSTRACT) && !parentIsVirtualClass(Ident(parentName), name))
          List({
            val volatileFixName = newTermName(volatileFixMap.get(name.toString).get)
            DefDef(Modifiers(DEFERRED), volatileFixName, List(), List(List()), tq"""Int""", EmptyTree)
          })
        else List()
      }

      //TODO: inherit correct abstract / impl members to fix volatile problem

      val volatileFixes: List[Tree] = {
        if (parents.length > 0 && volatileFixMap.get(parents(0).toString).isDefined && !parentIsVirtualClass(Ident(parentName), name))
          List({
            val volatileFixName = newTermName(volatileFixMap.get(parents(0).toString).get)
            DefDef(Modifiers(), volatileFixName, List(), List(List()), tq"""Int""", Literal(Constant(0)))
          })
        else List()
      }

      // TODO: fix getVolatileFixNameInParents, currently too many fixes get introduced
      val volatileFixesInParents: List[Tree] = {
        val vfnip = getVolatileFixNameInParents(inheritRel.map(newTypeName(_)), enclName, (if (parents.length > 0) getNameFromTree(parents(0)) else ""))
        if (vfnip.length > 0)
          List({
            val volatileFixName = vfnip.head
            DefDef(Modifiers(), volatileFixName, List(), List(List()), tq"""Int""", Literal(Constant(0)))
          })
        else
          List()
      }

      //println("volatileFixesInParents: " + volatileFixesInParents.mkString(" | "))

      constructorTransformed ++ volatileFixesIntro ++ volatileFixes ++ volatileFixesInParents
    }

    /**
     * we converted the class to a trait so change the constructor from <init> to $init$ and remove the super call...
     */
    def convertToTraitConstructor(templ: c.universe.Template, name: TypeName, tparams: List[TypeDef], bodies: List[Tree], mods: Modifiers, parentName: TypeName, enclName: TypeName, inheritRel: List[String]): c.universe.Template = {
      templ match {
        case Template(parents, self, body) =>
          //println("Template: (" + parents.mkString("|") + "," + self + "," + body.mkString(";"))
          //parents.map(p => Ident(newTypeName(virtualTraitName(getNameFromTree(p), parentName))))
          Template(List(tq"""scala.AnyRef"""), ValDef(Modifiers(PRIVATE), newTermName("self"), getTypeApplied(name, bodies), EmptyTree), transformVCBody(body, parents, bodies, name, mods, parentName, enclName, inheritRel))
      }
    }

    def typeTree(types: List[Tree]): c.universe.Tree = {
      if (types.length == 1)
        types(0)
      else
        CompoundTypeTree(Template(types, emptyValDef, List()))
    }

    def getNameFromSub(name: String) = name.takeRight(name.length - name.lastIndexOf("$") - 1)
    def getParentNameFromSub(name: String) = name.takeRight(name.length - name.indexOf("$") - 1).takeWhile(c => c != '$')

    def getNameFromTree(name: Tree) = {
      name match {
        case Ident(typeName) => typeName.toString
        case AppliedTypeTree(Ident(typeName), _) => typeName.toString
        case _ => name.toString
      }
    }

    def getInheritanceRelation(bodies: List[c.universe.Tree], vc_parents: List[Tree], name: TypeName, parents: List[Tree], enclName: TypeName): List[String] = {

      // family inheritance
      val family = if (!vc_parents.isEmpty)
        List(virtualTraitName(name, enclName))
      else
        List()

      val parentInheritance = getInheritanceTreeComplete(bodies, name, enclName, parents)

      //println("parents: " + showRaw(vc_parents))

      val ownInheritance = vc_parents.filter(p => {
        val parentName = getNameFromTree(p)
        bodies.exists(b =>
          b match {
            case ClassDef(mods, name, tparams, impl) => (isVirtualClass(mods) && parentName == name.toString)
            case _ => false
          })
      }).map(_.toString)

      //println("parentsString: " + vc_parents.mkString(" "))

      val all = (ownInheritance ++ parentInheritance ++ family).distinct // TODO: What is right linearization

      val res = if (all.length >= 2 && getNameFromSub(all.tail.head) != name.toString)
        getNameFromSub(all.tail.head) :: all.head :: all.tail.tail
      else
        all

      res
    }

    /*def getInheritanceTreeComplete(bodies: List[c.universe.Tree], className: TypeName, enclName: TypeName, parent: TypeName): List[String] = {
      val res = bodies.flatMap(b => b match {
        case ClassDef(mods, name, tparams, Template(parents, valDef, body)) if (name == className) =>
          virtualTraitName(name.toString, enclName) :: getInheritanceTreeComplete(bodies, parents(0).toString, enclName, parent)
        case _ => getInheritanceTreeInParents(className, parent).reverse
      })
      res
    }*/

    def getInheritanceTreeComplete(bodies: List[c.universe.Tree], className: TypeName, enclName: TypeName, parents: List[Tree]): List[String] = {
      val res = bodies.flatMap(b => b match {
        case ClassDef(mods, name, tparams, Template(vc_parents, valDef, body)) if (name == className) =>
          virtualTraitName(name.toString, enclName) :: getInheritanceTreeComplete(bodies, getNameFromTree(vc_parents(0)), enclName, parents)
        case _ => getInheritanceTreeInParents(className, parents).reverse
      })
      res.distinct
    }

    def getInheritanceTreeInParents(className: TypeName, parents: List[Tree]): List[String] = {
      parents.flatMap { parent =>
        try {
          val tpt = Select(Ident(parent.toString), newTypeName(finalClassName(parent.toString)))
          val tp = computeType(tpt)
          val fixClassTp = tp.declaration(newTypeName(fixClassName(className, parent.toString)))
          if (tp != NoType && tp.baseClasses.length > 0 && fixClassTp.isClass) {
            fixClassTp.asClass.baseClasses.drop(1).dropRight(2).map(bc => bc.name.toString)
          } else
            Nil
        } catch {
          case e: Throwable => e.printStackTrace(); Nil
        }
      }.distinct
    }

    def getVCClassesSymbols(name: String): List[Symbol] = {
      try {
        val tpt = Ident(newTypeName(name))
        val tp = computeType(tpt)
        tp.members.filter(s => s.name.toString.startsWith("VC_TRAIT$")).toList
      } catch {
        case e: Throwable => e.printStackTrace(); Nil
      }
    }

    def getParentVCClasses(parent: String): List[String] = {
      getVCClassesSymbols(parent).map(s => getNameFromSub(s.name.toString))
    }

    def parentContains(parent: String, name: String) = {
      try {
        val tpt = Select(Ident(newTermName(parent)), newTypeName(finalClassName(parent)))
        val tp = computeType(tpt)
        val res = tp.declarations.exists(s => s.name.toString == name)
        //println("parentContains: " + parent + ";" + name + " = " + res)
        res
      } catch {
        case e: Throwable => e.printStackTrace(); false
      }
    }

    def getTypeParams(name: TypeName, bodies: List[Tree]) = {
      //println("getTypeParams: " + name)
      val res = bodies.map(b => b match {
        case ClassDef(_, n, tparams, impl) if (n.toString == getNameFromSub(name.toString)) =>
          //println("getTypeParams: " + tparams); 
          Some(tparams)
        case _ => None
      })
      val res2 = res.filter(o => o.isDefined)
      if (res2.size > 0)
        res2.head
      else
        None
    }

    def getTypeNames(tparams: List[TypeDef]) = {
      //println("getTypeNames: " + tparams.mkString(" | "))
      tparams.map(t => t match {
        case TypeDef(mods, name, tparams, rhs) => name
      })
    }

    def getTypeApplied(name: TypeName, bodies: List[Tree]) = {
      val typeParams = getTypeParams(getNameFromSub(name.toString), bodies)
      if (typeParams.isEmpty || typeParams.get.isEmpty)
        Ident(name)
      else
        AppliedTypeTree(Ident(name), getTypeNames(typeParams.get).map(t => Ident(t)))
    }

    def mapInheritanceRelation(inheritRel: List[String], bodies: List[Tree]) = {
      //println("mapInheritanceRelation: " + inheritRel.mkString(" | "))
      val inheritRelMapped = inheritRel.map(s =>
        getTypeApplied(s, bodies))
      //println("mapInheritanceRelationMapped: " + inheritRel.mkString(" | "))
      inheritRelMapped
    }

    def getMixinNeededTypes(parents: List[Tree]): List[String] = {
      var seenVirtualClasses: List[String] = List()
      var seenTwiceVirtualClasses: List[String] = List()
      parents.foreach {
        p =>
          val parentVC = getParentVCClasses(getNameFromTree(p))
          seenTwiceVirtualClasses = seenTwiceVirtualClasses ++ parentVC.dropWhile(e => !seenVirtualClasses.contains(e))
          seenVirtualClasses = seenVirtualClasses ++ parentVC
      }
      seenTwiceVirtualClasses.distinct
    }

    def declsInVCTrait(traitName: TypeName): List[String] = {
      getVCClassesSymbols(getParentNameFromSub(traitName.toString))
        .filter(s => s.isClass && s.name == traitName)
        .flatMap(s =>
          s.asClass.toType.declarations
            .filter(s => s.isMethod && !s.asMethod.isConstructor)
            .map(_.name.toString)
            .filter(s => !s.endsWith("_$eq") && !s.startsWith("volatileFix$")))
        .map(_.trim).distinct
    }

    /*def nameClashes(classParents: List[Tree], enclName: String, bodies: List[Tree]): Map[String, List[Tree]] = {
      println("nameClashes for: " + classParents.mkString(" "))
      var seen: List[String] = List()
      var result = Map[String, List[Tree]]()
      classParents.foreach {
        p =>
          val parent = getParentNameFromSub(getNameFromTree(p))
          if (parent != enclName) {
            val members = declsInVCTrait(getNameFromTree(p))
            //println(s"members in $p:" + members.mkString(" "))
            members.foreach { m =>
              if (seen.contains(m))
                if (result.contains(m))
                  result += (m -> (p :: result.get(m).get))
                else
                  result += (m -> List(p))
              seen = seen ++ members
            }
          } else {
            // TODO: search in bodies for clashes...
          }
      }
      result
    }*/

    def membersOf(cls: TypeName, bodies: List[Tree], noAbstract: Boolean = false): List[String] = {
      println("membersOf: " + cls)
      bodies.flatMap(
        t => t match {
          case cd @ ClassDef(mods, name, tparams, Template(parents, valDef, body)) if (isVirtualClass(mods) && name == cls) =>
            //println(name + " " + body)
            body.flatMap(b => b match {
              case dd @ DefDef(mods, defName, tparams, vparamss, tpt, rhs) if (defName != nme.CONSTRUCTOR && (!noAbstract || !mods.hasFlag(DEFERRED))) => {
                //println(mods)
                List(defName.toString)
              }
              case _ => List()
            })
          case _ => List()
        })
    }

    def nameClashesForVCClass(vc: TypeName, classParents: List[Tree], enclName: TypeName, bodies: List[Tree]): Map[String, TypeName] = {
      println("nameClashesForVCClass: " + vc + ": " + classParents.mkString(" "))
      var result = Map[String, TypeName]()
      var seen = List[String]()
      println("name:" + vc)
      println("enclName: " + enclName)

      val classes = (virtualTraitName(vc, enclName) :: classParents.map(getNameFromTree(_))).reverse.distinct
      println("classParents: " + classes.mkString(" "))

      classes.foreach {
        p =>
          println("handling " + p)
          val p_parent = getParentNameFromSub(p)
          val members_in_p = if (p_parent != enclName.toString) {
            declsInVCTrait(p)
          } else {
            membersOf(getNameFromSub(p), bodies, true)
          }
          println("members in p: " + members_in_p.mkString(" "))
          members_in_p.foreach { m =>
            if (!seen.contains(m))
              seen ::= m
            else {
              result += m -> newTypeName(p)
            }
          }
      }

      //val members = membersOf(vc, bodies, true)

      //println("members: " + members)

      result
    }

    //TODO: support mixin-composition recursively
    def transformBody(body: List[Tree], enclName: TypeName, parents: List[Tree]): List[Tree] = {
      body.foreach(b =>
        b match {
          case cd @ ClassDef(mods, name, tparams, impl) if (isVirtualClass(mods)) =>
            volatileFixMap.put(name.toString, c.fresh("volatileFix$"))
          case _ => ;
        })

      val bodyTransform = body.flatMap(b =>
        b match {
          case cd @ ClassDef(mods, name, tparams, impl) if (isVirtualClass(mods)) =>
            if (mods.hasFlag(TRAIT))
              c.error(cd.pos, "Only classes can be declared as virtual (they will be converted to traits though).")
            //TODO: support type parameters
            //if (!tparams.isEmpty)
            //  c.error(cd.pos, "Type parameters are currently not supported.")

            val Template(vc_parents, _, _) = impl

            val inheritRel = getInheritanceRelation(body, vc_parents, name, parents, enclName)
            val classInner = getInheritanceTreeComplete(body, name, enclName, parents)
            val inheritRelMapped = mapInheritanceRelation(inheritRel, body)
            //println("inheritRelMapped: " + inheritRelMapped.mkString(" | "))
            val typeDefInner: c.universe.Tree = typeTree(inheritRelMapped) //.filter(s => !parentIsVirtualClass(parent, name) || inheritRel.length < 3 || s != virtualTraitName(name, enclName)) // non-volatile perk, not needed any more?

            val classTmpl = convertToTraitConstructor(impl, name, tparams, body, mods, getNameFromTree(parents(0)), enclName, classInner)

            val b = List(
              TypeDef(Modifiers(DEFERRED), name, tparams, TypeBoundsTree(Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Null")), typeDefInner)),
              ClassDef(Modifiers(ABSTRACT | TRAIT), virtualTraitName(name, enclName), tparams, classTmpl))
            //println("mods: " + name.toString + ": " + mods.toString + " = " + mods.hasFlag(ABSTRACT))
            if (parentIsVirtualClass(parents(0), name) || (mods.hasFlag(ABSTRACT)))
              b
            else
              ModuleDef(Modifiers(), name.toTermName, Template(List(Select(Ident("scala"), newTypeName("AnyRef"))), emptyValDef, List(DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))), DefDef(Modifiers(), newTermName("apply"), List(), List(List()), TypeTree(), Ident(newTermName(factoryName(name))))))) ::
                DefDef(Modifiers(DEFERRED), newTermName(factoryName(name)), tparams, List(), getTypeApplied(name, body), EmptyTree) ::
                b
          case DefDef(mods, name, tparams, vparamss, tpt, rhs) if (name.toString == "<init>") =>
            List(noParameterTraitConstructor)
          case _ => List(b)
        })
      val mixinParents = getMixinNeededTypes(parents)
      //println("mixinParents: " + mixinParents.mkString(" "))
      val mixinTransform = mixinParents.map(p => {
        val inheritRel = getInheritanceRelation(body, List(), p, parents, enclName)
        val inheritRelMapped = mapInheritanceRelation(inheritRel, body)
        //println("inheritRelMapped: " + inheritRelMapped.mkString(" | "))
        val typeDefInner: c.universe.Tree = typeTree(inheritRelMapped) //.filter(s => !parentIsVirtualClass(parent, name) || inheritRel.length < 3 || s != virtualTraitName(name, enclName)) // non-volatile perk, not needed any more?

        TypeDef(Modifiers(DEFERRED), p, List(), TypeBoundsTree(Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Null")), typeDefInner))
      })
      bodyTransform ++ mixinTransform
    }

    def makeFinalVirtualClassPart(name: TypeName, enclName: TypeName, mods: Modifiers, typeDef: Tree, tparams: List[TypeDef], classParents: List[Tree], nameClashes: Map[String, TypeName]): List[Tree] = {
      //println("makeFinalVirtualClassPart: " + name.toString + " | " + typeDef.toString + " | " + classParents.mkString(" --- "))
      //DefDef(Modifiers(), newTermName("test"), List(), List(), TypeTree(), Select(Super(This(tpnme.EMPTY), newTypeName("AnyRef")), newTermName("test")))))

      val clashOverrides = nameClashes.map(s => DefDef(Modifiers(OVERRIDE), newTermName(s._1), List(), List(), TypeTree(), Select(Super(This(tpnme.EMPTY), s._2), newTermName(s._1))))

      println("clashOverrides: " + clashOverrides.mkString(" ------- "))

      val fL = List(TypeDef(Modifiers(), name, tparams, typeDef),
        ClassDef(mods, fixClassName(name, enclName), tparams, Template(classParents, emptyValDef, List(noParameterConstructor) ++ clashOverrides)))

      val fixClassTypeName = if (tparams.isEmpty)
        Ident(newTypeName(fixClassName(name, enclName)))
      else
        AppliedTypeTree(Ident(newTypeName(fixClassName(name, enclName))), getTypeNames(tparams).map(t => Ident(t)))

      if (!(mods.hasFlag(ABSTRACT)))
        DefDef(Modifiers(), newTermName(factoryName(name)), tparams, List(), TypeTree(), Apply(Select(New(fixClassTypeName), nme.CONSTRUCTOR), List())) ::
          fL
      else
        fL
    }

    def finalClassBodyContainsVCClass(body: List[Tree], name: String) = {
      body.exists(t => t match {
        case ClassDef(_, n, _, _) => name == getNameFromSub(n.toString)
        case _ => false
      })
    }

    def bodyContains(body: List[Tree], name: String) = {
      body.exists(t => t match {
        case ClassDef(_, n, _, _) => name == n.toString
        case _ => false
      })
    }

    def finalClass(enclName: TypeName, body: List[c.universe.Tree], parents: List[Tree]) = {
      val finalClassBody: List[c.universe.Tree] = noParameterConstructor :: body.flatMap(b =>
        b match {
          case ClassDef(mods, name, tparams, impl) if (isVirtualClass(mods)) =>
            // TODO add possibly missing typeDefs...
            val Template(vc_parents, _, _) = impl

            val typeDefInner = typeTree(mapInheritanceRelation(getInheritanceRelation(body, vc_parents, name, parents, enclName), body))

            val classInner = getInheritanceTreeComplete(body, name, enclName, parents)

            val classAdditions = classInner.flatMap(p => if (!classInner.contains(virtualTraitName(getNameFromSub(p.toString), enclName)) &&
              finalClassBodyContainsVCClass(body, getNameFromSub(p.toString)))
              List(virtualTraitName(getNameFromSub(p.toString), enclName))
            else
              List())

            val classParents = mapInheritanceRelation((classInner ++ classAdditions).distinct, body)

            val nc = nameClashesForVCClass(name, classParents, enclName, body)

            println("nameClashes: " + nc.mkString(" "))

            makeFinalVirtualClassPart(name, enclName, mods, typeDefInner, tparams, classParents, nc)

          case _ => Nil
        })

      val toCompleteFromParents = parents.flatMap(p => getParentVCClasses(getNameFromTree(p))).filter(!finalClassBodyContainsVCClass(finalClassBody, _)).distinct

      val bodyCompletion = toCompleteFromParents.flatMap { name =>
        val inheritance = getInheritanceTreeInParents(name, parents)
        val missing = inheritance.flatMap(s =>
          if (bodyContains(body, getNameFromSub(s)))
            List(virtualTraitName(getNameFromSub(s), enclName))
          else
            List()).distinct

        val typeInner = typeTree((inheritance ++ missing).map(t => Ident(newTypeName(t))))

        val mods = if (parentContains(parents(0).toString, factoryName(name)))
          Modifiers()
        else
          Modifiers(ABSTRACT)

        //val nc = nameClashes(parents, enclName.toString, body)

        val classParents = (inheritance ++ missing).map(s => Ident(newTypeName(s)))
        val nc = nameClashesForVCClass(name, classParents, enclName, body)
        makeFinalVirtualClassPart(name, enclName, mods, typeInner, List(), classParents, nc)
      }

      val tmpl = Template(List(Ident(enclName)), emptyValDef, finalClassBody ++ bodyCompletion)

      ClassDef(Modifiers(), finalClassName(enclName), List(), tmpl)
    }

    val result: c.Tree = {
      annottees.map(_.tree).toList match {
        case (cd @ ClassDef(mods, name, tparams, Template(parents, self, body))) :: rest =>
          val classDef = ClassDef(Modifiers(ABSTRACT | TRAIT), name, tparams, Template(parents, self, transformBody(body, name, parents)))
          // def <init>() = super.<init>()
          // val objectConstructor =
          //  q"""def ${nme.CONSTRUCTOR}() = { super.${nme.CONSTRUCTOR}(); () }"""
          val newObjectBody: List[Tree] = noParameterConstructor :: finalClass(name, body, parents) :: DefDef(Modifiers(), newTermName("apply"), List(), List(List()), TypeTree(), Apply(Select(New(Ident(newTypeName(finalClassName(name)))), nme.CONSTRUCTOR), List())) :: Nil
          val newObjectTemplate = Template(List(tq"""scala.AnyRef"""), emptyValDef, newObjectBody)
          val newObjectDef = ModuleDef(Modifiers(), name.toTermName, newObjectTemplate)
          Block(List(classDef, newObjectDef), Literal(Constant()))
      }
    }
    c.Expr[Any](result)
  }
}

class virtualContext extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro virtualContext.impl
}

object virtualMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._
    val result = {
      annottees.map(_.tree).toList match {
        case ClassDef(mods, name, tparams, impl) :: Nil =>
          ClassDef(mods, name, tparams, impl)
      }
    }
    c.Expr[Any](result)
  }
}

class virtual extends StaticAnnotation {
  //def macroTransform(annottees: Any*) = macro virtualMacro.impl
}

class virtualOverride extends StaticAnnotation {

}

object printMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._

    val s = show(annottees(0).tree)
    println(s)
    annottees(0)
  }
}

class print extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro printMacro.impl
}