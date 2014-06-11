package VirtualClasses

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.collection.mutable.HashMap

object family {

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._

    ////////// Helper

    // introduce paramaccessor as flag because macros don't provide it.
    val PARAMACCESSOR = (1 << 29).toLong.asInstanceOf[FlagSet]

    ///////////

    /////////// poor endless loop prevention...

    if (c.enclosingMacros.size > 1000) {
      println(s"Macro has recursed too many times. Aborting. ${c.enclosingMacros}")
      c.abort(c.macroApplication.pos, "Macro has recursed too many times. Aborting.")
    }

    //////////

    def virtualTraitPrefix = "VC_TRAIT"
    def fixClassPrefix = "VC_FIX"
    def finalClassPrefix = "VC_FINAL"

    def virtualTraitName(className: Name, enclClassName: Name): Name =
      newTypeName(virtualTraitPrefix + "$" + enclClassName + "$" + className)
    def factoryName(className: Name): Name =
      className
    def fixClassName(className: Name, enclClassName: Name): Name =
      newTypeName(fixClassPrefix + "$" + enclClassName + "$" + className)
    def finalClassName(className: Name): Name =
      newTypeName(finalClassPrefix + "$" + className)

    lazy val noParameterConstructor = q"def ${nme.CONSTRUCTOR}() = { super.${nme.CONSTRUCTOR}(); () }"
    lazy val noParameterTraitConstructor = DefDef(Modifiers(), newTermName("$init$"), List(), List(List()), TypeTree(), Block(List(), Literal(Constant(()))))

    def parameterConstructor(params: List[(TermName, TypeName)]) = {
      val consparams = params.map { case (name, tpe) => ValDef(Modifiers(PARAM | PARAMACCESSOR), name, Ident(tpe), EmptyTree) }
      q"def ${nme.CONSTRUCTOR}(..$consparams) = { super.${nme.CONSTRUCTOR}(); () }"
    }

    def isVirtualClass(mods: c.universe.Modifiers) = {
      mods.annotations.foldRight(false)((a, b) => b || (a equalsStructure q"new virtual()"))
    }

    def isAbstract(mods: c.universe.Modifiers) =
      (mods.hasFlag(ABSTRACT) || mods.hasFlag(ABSOVERRIDE))

    def isOverriden(mods: c.universe.Modifiers) =
      (mods.hasFlag(OVERRIDE) || mods.hasFlag(ABSOVERRIDE))

    def isOverridenVirtualClass(mods: c.universe.Modifiers) = {
      isVirtualClass(mods) && isOverriden(mods)
    }

    def parentContainsVirtualClass(parent: Tree, virtualClass: TypeName) = {
      computeType(parent).declarations.exists(s => s.name == virtualTraitName(virtualClass, getNameFromTree(parent).toTypeName))
    }

    def getParentsInParent(parent: TypeName, name: TypeName) = {
      val tpe = computeType(Ident(parent)).member(name)
      val hi = tpe.typeSignature.asInstanceOf[scala.reflect.internal.Types#Type].bounds.hi
      val tpe_parents = hi.parents.map(_.typeSymbol.name.toTypeName.asInstanceOf[c.universe.TypeName])
      (if (tpe_parents.size == 1)
        List(hi.typeSymbol.name.toTypeName.asInstanceOf[c.universe.TypeName])
      else
        tpe_parents)
        .filter(!_.toString.startsWith(virtualTraitPrefix + "$"))
        .filter(p => p != newTypeName("scala.AnyRef") && p != newTypeName("Object"))
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

    lazy val typeCache = collection.mutable.Map[String, Type]()

    def computeType(tpt: Tree): Type = {
      if (typeCache.contains(tpt.toString)) {
        typeCache(tpt.toString)
      } else {
        val tpe = computedType(tpt)
        typeCache(tpt.toString) = tpe
        tpe
      }
    }

    def computedType(tpt: Tree): Type = {
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

    def typeTree(types: List[Tree]): c.universe.Tree = {
      CompoundTypeTree(Template(tq"scala.AnyRef" :: types, emptyValDef, List()))
    }

    def getNameFromSub(name: Name): Name = newTypeName(name.toString.takeRight(name.toString.length - name.toString.lastIndexOf("$") - 1))
    def getParentNameFromSub(name: Name): Name = newTypeName(name.toString.takeRight(name.toString.length - name.toString.indexOf("$") - 1).takeWhile(c => c != '$'))

    def getNameFromTree(name: Tree): Name = {
      name match {
        case Ident(typeName) => typeName
        case AppliedTypeTree(Ident(typeName), _) => typeName
        case _ => newTypeName(name.toString)
      }
    }

    def parentContains(parent: Name, name: TermName) = {
      try {
        val tpt = Select(Ident(parent.toTermName), finalClassName(parent.toTypeName))
        val tp = computeType(tpt)
        val res = tp.members.exists(s => s.name == name)
        res
      } catch {
        case e: Throwable => e.printStackTrace(); false
      }
    }

    def getTypeParams(name: TypeName, bodies: List[Tree]) = {
      val res = bodies.map(b => b match {
        case ClassDef(_, n, tparams, impl) if (n == getNameFromSub(name)) =>
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
      tparams.map(t => t match {
        case TypeDef(mods, name, tparams, rhs) => name
      })
    }

    def getTypeApplied(name: TypeName, bodies: List[Tree]) = {
      val typeParams = getTypeParams(getNameFromSub(name).toTypeName, bodies)
      if (typeParams.isEmpty || typeParams.get.isEmpty)
        Ident(name)
      else
        AppliedTypeTree(Ident(name), getTypeNames(typeParams.get).map(t => Ident(t)))
    }

    def mapInheritanceRelation(inheritRel: List[TypeName], bodies: List[Tree]) = {
      val inheritRelMapped = inheritRel.map(s =>
        getTypeApplied(s, bodies))
      inheritRelMapped
    }

    def membersOf(cls: TypeName, bodies: List[Tree], noAbstract: Boolean = false): List[Name] = {
      bodies.flatMap(
        t => t match {
          case cd @ ClassDef(mods, name, tparams, Template(parents, valDef, body)) if (isVirtualClass(mods) && name == cls) =>
            body.flatMap(b => b match {
              case dd @ DefDef(mods, defName, tparams, vparamss, tpt, rhs) if (defName != nme.CONSTRUCTOR && (!noAbstract || !mods.hasFlag(DEFERRED))) => {
                List(defName)
              }
              case _ => List()
            })
          case _ => List()
        })
    }

    def transformBody(vcc: VCContext): List[Tree] = {
      val ClassDef(_, enclName, _, Template(parents, _, body)) = vcc.familyClassDef

      val bodyTransform = body.flatMap(b =>
        b match {
          case cd @ ClassDef(mods, name, tparams, impl) if (isVirtualClass(mods)) =>
            if (mods.hasFlag(TRAIT))
              c.error(cd.pos, "Only classes can be declared as virtual (they will be converted to traits though).")
            //TODO: support type parameters
            if (!tparams.isEmpty)
              c.abort(cd.pos, "Type parameters are currently not supported.")

            if (!isOverridenVirtualClass(mods) && parents.exists(t => parentContainsVirtualClass(t, name)))
              c.abort(cd.pos, s"The following parents of the class family already declare a class with the name $name: ${parents.filter(t => parentContainsVirtualClass(t, name)).mkString(", ")}.\nTo override functionality, declare the virtual class as overriden.")

            val Template(vc_parents, _, vc_body) = impl

            val constructorParameters = vcc.getConstructorParameters(name)

            val mixed: List[(TermName, TypeName)] = vcc.getMixedConstructorParameters(name)

            if (isOverridenVirtualClass(mods) && !vcc.getConstructorParametersInBodies(vc_body).isEmpty && !mixed.zip(vcc.getConstructorParametersInBodies(vc_body)).map(a => a._1._1 == a._2._1 && a._1._2 == a._2._2).foldRight(true)((a, b) => a && b))
              c.abort(cd.pos, "Overriden virtual classes may only add constructor parameters at the end.\nThe constructor parameters have to start with:\n" +
                mixed.map(cp => s"${cp._1}: ${cp._2}").mkString(", "))

            val inheritRel = vcc.getTypeBounds(name)
            val classInner = vcc.getClassMixins(name)
            val inheritRelMapped = mapInheritanceRelation(inheritRel, body)
            val typeDefInner: c.universe.Tree =
              typeTree(inheritRelMapped)

            val classTmpl = vcc.convertToTraitConstructor(cd)

            val list = if (isAbstract(mods)) List() else constructorParameters.flatMap { cp =>
              val vparamss = List(
                cp.map { case (name, tpe) => ValDef(Modifiers(PARAM), name, Ident(tpe), EmptyTree) })
              List[Tree](DefDef(Modifiers(DEFERRED), factoryName(name).toTermName, tparams, vparamss, getTypeApplied(name, body), EmptyTree))
            }

            List(
              TypeDef(Modifiers(DEFERRED), name, tparams, TypeBoundsTree(Select(Select(Ident(nme.ROOTPKG), newTermName("scala")), newTypeName("Null")), typeDefInner)),
              ClassDef(Modifiers(ABSTRACT | TRAIT), virtualTraitName(name, enclName).toTypeName, tparams, classTmpl)) ++ list

          case DefDef(mods, name, tparams, vparamss, tpt, rhs) if (name.toString == "<init>") =>
            List()
          case _ => List(b)
        })

      val bodyCompletion = vcc.toCompleteFromParents.map { name =>
        val typeDef = vcc.getTypeBounds(name.toTypeName)

        TypeDef(Modifiers(DEFERRED), name.toTypeName, List(), TypeBoundsTree(Select(Select(Ident(nme.ROOTPKG), newTermName("scala")), newTypeName("Null")), typeTree(typeDef.map(Ident(_)))))
      }

      bodyTransform ++ bodyCompletion
    }

    def makeFinalVirtualClassPart(name: TypeName, enclName: TypeName, mods: Modifiers, typeDef: Tree, tparams: List[TypeDef], classParents: List[Tree], constructorParameters: List[List[(TermName, TypeName)]]): List[Tree] = {
      val fcn = fixClassName(name, enclName)

      val fixMods = if (isAbstract(mods)) Modifiers(ABSTRACT) else NoMods

      val td = TypeDef(Modifiers(), name, tparams, typeDef)

      val fixClassTypeName = if (tparams.isEmpty)
        Ident(fixClassName(name, enclName).toTypeName)
      else
        AppliedTypeTree(Ident(fixClassName(name, enclName).toTypeName), getTypeNames(tparams).map(t => Ident(t)))

      if (!(isAbstract(mods))) {
        var counter = 0
        val list = constructorParameters.flatMap { cp =>
          val vparamss = List(
            cp.map { case (name, tpe) => ValDef(Modifiers(PARAM), name, Ident(tpe), EmptyTree) })
          counter += 1
          val fixName = newTermName(fixClassName(name, enclName) + "$" + counter)
          List[Tree](DefDef(Modifiers(), factoryName(name), tparams, vparamss, TypeTree(), Apply(Select(New(Ident(fixName.toTypeName)), nme.CONSTRUCTOR), cp.map { case (name, tpe) => Ident(name) })),
            ClassDef(fixMods, fixName.toTypeName, tparams, Template(classParents, emptyValDef, cp.map { case (name, tpe) => ValDef(Modifiers(PARAMACCESSOR | OVERRIDE), name, Ident(tpe), EmptyTree) } ++ List(parameterConstructor(cp)))))
        }
        td :: list
      } else
        List(td)
    }

    def finalClass(vcc: VCContext) = {
      val ClassDef(_, enclName, _, Template(parents, _, body)) = vcc.familyClassDef

      val finalClassBody: List[c.universe.Tree] = noParameterConstructor :: body.flatMap(b =>
        b match {
          case ClassDef(mods, name, tparams, Template(vc_parents, _, vc_body)) if (isVirtualClass(mods)) =>
            val clasLin = vcc.getVirtualClassLinearization(name).reverse
            val classInner = vcc.getClassMixins(name)

            val typeDefInner = typeTree(vcc.getTypeBounds(name).map(Ident(_))) //typeTree(mapInheritanceRelation(getInheritanceRelation(body, vc_parents, name, parents, enclName), body))

            val classParents = mapInheritanceRelation(classInner.distinct, body)

            val constructorParameters = vcc.getConstructorParameters(name)

            makeFinalVirtualClassPart(name, enclName, mods, typeDefInner, tparams, classParents, constructorParameters)

          case _ => Nil
        })

      val bodyCompletion = vcc.toCompleteFromParents.flatMap { name =>
        val typeDef = vcc.getTypeBounds(name.toTypeName).map(Ident(_))

        val classParents = vcc.getClassMixins(name.toTypeName).map(Ident(_))

        val mods = if (vcc.allBaseClasses.exists(p => parentContains(p, factoryName(name).toTermName)))
          Modifiers()
        else
          Modifiers(ABSTRACT)

        val constructorParametersWithEmpty = parents.flatMap(p => vcc.getConstructorParametersInParent(name, getNameFromTree(p).toTypeName)).distinct
        val constructorParametersWithoutEmpty = constructorParametersWithEmpty.filter(cp => !cp.isEmpty)
        val constructorParameters = if (constructorParametersWithoutEmpty.isEmpty) constructorParametersWithEmpty else constructorParametersWithoutEmpty

        makeFinalVirtualClassPart(name.toTypeName, enclName, mods, typeTree(typeDef), List(), classParents, constructorParameters)
      }
      val tmpl = Template(List(Ident(enclName)), emptyValDef, finalClassBody ++ bodyCompletion)

      ClassDef(Modifiers(), finalClassName(enclName).toTypeName, List(), tmpl)
    }

    case class VCContext(familyClassDef: ClassDef) {
      val ClassDef(_, enclName, _, Template(parents, _, body)) = familyClassDef

      def getVCClassesSymbols(name: Name): List[Symbol] = {
        try {
          val tpt = Ident(name.toTypeName)
          val tp = computeType(tpt)
          tp.members.filter(s => s.name.toString.startsWith(virtualTraitPrefix + "$")).toList
        } catch {
          case e: Throwable => e.printStackTrace(); Nil
        }
      }

      def getParentVCClasses(parent: Name): List[Name] = {
        getVCClassesSymbols(parent).map(s => getNameFromSub(s.name))
      }

      def isVCInBodies(name: TypeName) = {
        body.exists(b => b match {
          case ClassDef(mods, n, _, _) => (isVirtualClass(mods) || isOverridenVirtualClass(mods)) && n == name
          case _ => false
        })
      }

      def isVC(name: TypeName) = {
        isVCInBodies(name) || allBaseClasses.exists(p => parentContainsVirtualClass(Ident(p), name))
      }

      def findClassInBodies(name: TypeName) = {
        body.find(b => b match {
          case ClassDef(_, n, _, _) => name == n
          case _ => false
        })
      }

      def getVCBody(name: TypeName) = {
        findClassInBodies(name) match {
          case None => None
          case Some(cd) => cd match {
            case ClassDef(_, _, _, Template(_, _, body)) => Some(body)
          }
        }
      }

      def getTypeBounds(name: TypeName) = {
        (getVCParents(name) ++
          getVCTraits(name).reverse).distinct
      }

      def getParentsInParents(name: TypeName) = {
        this.parents.flatMap(p => getParentsInParent(getNameFromTree(p).toTypeName, name))
      }

      def getVCParents(name: TypeName) = {
        (getBaseClassesInBodies(name).filter(p => p != newTypeName("scala.AnyRef") && p != newTypeName("Object")) ++ getParentsInParents(name)).distinct
      }

      lazy val allBaseClasses: List[TypeName] = parents.flatMap(p => { computeType(p).baseClasses.filter(n => !List("scala.AnyRef", "Any", "Object").contains(n.toString)).map(_.name.toTypeName) }).reverse.distinct.reverse

      lazy val toCompleteFromParents: List[TypeName] = parents.flatMap(p => getParentVCClasses(getNameFromTree(p).toTypeName).map(_.toTypeName)).filter(n => !findClassInBodies(n.toTypeName).isDefined).distinct

      def getBaseClassesInBodies(name: TypeName): List[TypeName] = {
        body.flatMap { b =>
          b match {
            case ClassDef(_, n, _, Template(vc_parents, _, _)) if (n == name) => vc_parents.map(t => getNameFromTree(t).toTypeName)
            case _ => List()
          }
        }.filter(p => p.toString != "scala.AnyRef")
      }

      def getVirtualClassLinearization(name: TypeName): List[TypeName] = {
        val parents = getBaseClassesInBodies(name)
        val current = (allBaseClasses.flatMap(p => getParentsInParent(p, name)) ++ parents.reverse).filter(n => n.toString != "scala.AnyRef").map(_.toString).distinct.map(newTypeName(_))
        name :: current.flatMap(c => getVirtualClassLinearization(c)).reverse.distinct.reverse
      }

      def getVCTraits(name: TypeName): List[TypeName] = {
        val inParents = allBaseClasses.filter(p => parentContainsVirtualClass(Ident(p), name)).map(p => virtualTraitName(name, p).toTypeName)
        val result = if (findClassInBodies(name).isDefined) virtualTraitName(name, enclName).toTypeName :: inParents else inParents
        if (result.isEmpty)
          List(name)
        else
          result
      }

      def getClassMixins(name: TypeName): List[TypeName] = {
        getVirtualClassLinearization(name).reverse.flatMap(n => getVCTraits(n).reverse)
      }

      def getConstructorParametersInBodies(vc_body: List[Tree]) = {
        val constructorParameters = vc_body.flatMap(b => b match {
          case ValDef(mods, name, Ident(tn), _) if mods.hasFlag(PARAMACCESSOR) => List((name, tn.toTypeName))
          case vd @ ValDef(mods, name, _, _) if mods.hasFlag(PARAMACCESSOR) => c.abort(vd.pos, s"Constructor parameter $name cannot have type parameters.")
          case _ => List()
        })
        constructorParameters
      }

      def getConstructorParametersInParent(vc_name: Name, parent: TypeName) = {
        val factorySym = computeType(tq"$parent").member(factoryName(vc_name).toTermName)
        val res = if (factorySym == NoSymbol)
          List()
        else {
          factorySym.asTerm.alternatives.map(alt => if (alt.isMethod) alt.asMethod.paramss.head.map {
            s => (s.name.toTermName, s.typeSignature.typeSymbol.name.toTypeName)
          }
          else List())
        }
        //println(s"getConstructorParametersInParent($vc_name, $parent) = $res")
        res
      }

      def getLongestConstructorParametersInParent(vc_name: Name, parent: TypeName) = {
        var longest = List[(TermName, TypeName)]()
        getConstructorParametersInParent(vc_name, parent).foreach(l => {
          if (l.length > longest.length)
            longest = l
        })
        longest
      }

      def getMixedConstructorParameters(name: TypeName) = {
        var allLongestConstructors = parents.map { p =>
          val classLin = getVirtualClassLinearization(name)
          getLongestConstructorParametersInParent(name, getNameFromTree(p).toTypeName)
        }.distinct
        var mixed: List[(TermName, TypeName)] = List()
        if (allLongestConstructors.length > 0) {
          while (!allLongestConstructors.isEmpty) {
            val head = allLongestConstructors.head
            if (head.isEmpty) {
              allLongestConstructors = allLongestConstructors.tail
            } else {
              val firstParameter = head.head
              mixed ::= firstParameter
              allLongestConstructors = allLongestConstructors.map(cp => if (!cp.isEmpty && cp.head == firstParameter) cp.tail else cp)
            }
          }
          mixed.reverse
        } else List()
      }

      def getConstructorParameters(name: TypeName): List[List[(TermName, TypeName)]] = {
        val classLin = getVirtualClassLinearization(name)
        val res = classLin.flatMap(n => getConstructorParametersForVC(n)).distinct
        //println(s"getConstructorParameters($name) = $res")
        if (res.filter(cp => !cp.isEmpty).isEmpty)
          res
        else
          res.filter(cp => !cp.isEmpty)
      }

      def getConstructorParametersForVC(name: TypeName): List[List[(TermName, TypeName)]] = {
        val constructorParametersWithEmpty = (List(getConstructorParametersInBodies(getVCBody(name).getOrElse(List()))) ++ List(getMixedConstructorParameters(name)) ++ parents.flatMap(p => getConstructorParametersInParent(name, getNameFromTree(p).toTypeName))).distinct
        val constructorParametersWithoutEmpty = constructorParametersWithEmpty.filter(cp => !cp.isEmpty)
        val constructorParameters = if (constructorParametersWithoutEmpty.isEmpty) constructorParametersWithEmpty else constructorParametersWithoutEmpty
        //println(s"getConstructorParametersForVC($name) = ${constructorParameters.distinct}")
        constructorParameters.distinct
      }

      def isConstructorParmeterOverriden(vc_name: TypeName, cp_name: TermName) = {
        val constructorParameters = getConstructorParameters(vc_name)
        val constructorParametersFiltered = constructorParameters.map(l => l.filter(p => p._1 == cp_name))
        constructorParametersFiltered.filter(l => !l.isEmpty).length > 1
      }

      /**
       * we converted the class to a trait so change the constructor from <init> to $init$ and remove the super call...
       */
      def convertToTraitConstructor(virtualClassDef: ClassDef): c.universe.Template = {
        virtualClassDef.impl match {
          case Template(vc_parents, self, body) =>
            Template(tq"""scala.AnyRef""" :: getClassMixins(virtualClassDef.name).filter(cn => cn != virtualTraitName(virtualClassDef.name, enclName)).map(cn => Ident(cn)), ValDef(Modifiers(PRIVATE), newTermName("self"), getTypeApplied(virtualClassDef.name, body), EmptyTree), transformVCBody(virtualClassDef))
        }
      }

      def transformVCBody(virtualClassDef: ClassDef) = {
        val ClassDef(mods, vc_name, _, Template(vc_parents, _, body)) = virtualClassDef
        val constructorTransformed = body.flatMap(d => d match {
          case DefDef(mods, name, tparams, vparamss, tpt, rhs) if (name.toString == "<init>") =>
            List(noParameterTraitConstructor) ++ vparamss.head.map(p => p match {
              case ValDef(mods, name, tparams, rhs) => {
                //val newMods = Modifiers((mods.flags.asInstanceOf[Long] & ~PARAMACCESSOR.asInstanceOf[Long]).asInstanceOf[FlagSet])

                val deferredFlags = if (rhs == EmptyTree) DEFERRED else NoFlags
                val overrideFlags = if (isConstructorParmeterOverriden(vc_name, name)) OVERRIDE else NoFlags
                ValDef(Modifiers(deferredFlags | overrideFlags), name, tparams, rhs)
              }
            })
          case ValDef(mods, name, tparams, rhs) if (mods.hasFlag(PARAMACCESSOR)) => List() //List(ValDef(Modifiers(DEFERRED), name, tparams, EmptyTree))
          case _ => List(d)
        })

        constructorTransformed
      }
    }

    // main transformation happens here
    val result: c.Tree = {
      annottees.map(_.tree).toList match {
        case (cd @ ClassDef(mods, name, tparams, Template(parents, self, body))) :: rest =>
          val vcc = new VCContext(cd)
          val classDef =
            q"""abstract trait $name[..$tparams] 
          		  extends ..$parents { outer => 
          		  ..${transformBody(vcc)} 
                }"""
          val newObjectDef =
            q"""object ${name.toTermName} { 
          		def apply() = new ${finalClassName(name).toTypeName}; 
          		${finalClass(vcc)} 
          	}"""

          q"{ $classDef; $newObjectDef }"
      }
    }
    c.Expr[Any](result)
  }
}

class family extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro family.impl
}

class virtual extends StaticAnnotation

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