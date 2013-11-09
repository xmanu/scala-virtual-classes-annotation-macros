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

    def noParameterConstructor = DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(()))))
    def noParameterTraitConstructor = DefDef(Modifiers(), newTermName("$init$"), List(), List(List()), TypeTree(), Block(List(), Literal(Constant(()))))

    def isVirtualClass(mods: c.universe.Modifiers) = {
      mods.annotations.foldRight(false)((a, b) => b || (a.toString == "new virtual()"))
    }

    def parentIsVirtualClass(parent: Tree, virtualClass: TypeName) = {
      computeType(parent).members.exists(s => s.name.decoded.startsWith("VC_TRAIT$" + parent.toString + "$" + virtualClass.toString))
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

    /**
     * we converted the class to a trait so change the constructor from <init> to $init$ and remove the super call...
     */
    def convertToTraitConstructor(templ: c.universe.Template, name: TypeName, tparams: List[TypeDef], bodies: List[Tree]): c.universe.Template = {
      templ match {
        case Template(parents, self, body) =>
          //println("Template: (" + parents.mkString("|") + "," + self + "," + body.mkString(";"))
          Template(List(tq"""scala.AnyRef"""), ValDef(Modifiers(PRIVATE), newTermName("self"), getTypeApplied(name, bodies), EmptyTree), body.map(d => d match {
            case DefDef(mods, name, tparams, vparamss, tpt, rhs) if (name.toString == "<init>") =>
              noParameterTraitConstructor
            case _ => d
          }))
      }
    }

    def typeTree(types: List[Tree]): c.universe.Tree = {
      if (types.length == 1)
        types(0)
      else
        CompoundTypeTree(Template(types, emptyValDef, List()))
    }

    def getNameFromSub(name: String) = name.takeRight(name.length - name.lastIndexOf("$") - 1)

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

    def getParentVCClasses(parent: String): List[String] = {
      try {
        val tpt = Select(Ident(newTermName(parent)), newTypeName(finalClassName(parent)))
        val tp = computeType(tpt)
        tp.members.filter(s => s.name.toString.startsWith("VC_TRAIT$")).map(s => getNameFromSub(s.name.toString)).toList
      } catch {
        case e: Throwable => e.printStackTrace(); Nil
      }
    }

    def parentContains(parent: String, name: String) = {
      try {
        val tpt = Select(Ident(newTermName(parent)), newTypeName(finalClassName(parent)))
        val tp = computeType(tpt)
        val res = tp.members.exists(s => s.name.toString == name)
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
    
    def getMixinNeededTypes(parents: List[Tree]) : List[String] = {
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

    //TODO: support mixin-composition
    def transformBody(body: List[Tree], enclName: TypeName, parents: List[Tree]): List[Tree] = {
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
            val inheritRelMapped = mapInheritanceRelation(inheritRel, body)
            //println("inheritRelMapped: " + inheritRelMapped.mkString(" | "))
            val typeDefInner: c.universe.Tree = typeTree(inheritRelMapped) //.filter(s => !parentIsVirtualClass(parent, name) || inheritRel.length < 3 || s != virtualTraitName(name, enclName)) // non-volatile perk, not needed any more?

            val b = List(
              TypeDef(Modifiers(DEFERRED), name, tparams, TypeBoundsTree(Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Null")), typeDefInner)),
              ClassDef(Modifiers(ABSTRACT | TRAIT), virtualTraitName(name, enclName), tparams, convertToTraitConstructor(impl, name, tparams, body)))
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

    def makeFinalVirtualClassPart(name: TypeName, enclName: TypeName, mods: Modifiers, typeDef: Tree, tparams: List[TypeDef], classParents: List[Tree]): List[Tree] = {
      //println("makeFinalVirtualClassPart: " + name.toString + " | " + typeDef.toString + " | " + classParents.mkString(" --- "))

      val fL = List(TypeDef(Modifiers(), name, tparams, typeDef),
        ClassDef(mods, fixClassName(name, enclName), tparams, Template(classParents, emptyValDef, List(noParameterConstructor))))

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

            makeFinalVirtualClassPart(name, enclName, mods, typeDefInner, tparams, mapInheritanceRelation((classInner ++ classAdditions).distinct, body))

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

        makeFinalVirtualClassPart(name, enclName, mods, typeInner, List(), (inheritance ++ missing).map(s => Ident(newTypeName(s))))
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