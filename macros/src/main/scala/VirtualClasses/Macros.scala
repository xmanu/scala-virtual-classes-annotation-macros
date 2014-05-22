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

    def getConstructorParameters(vc_body: List[Tree]) = {
      val constructorParameters = vc_body.flatMap(b => b match {
        case ValDef(mods, name, Ident(tn), _) if mods.hasFlag(PARAMACCESSOR) => List((name, tn.toTypeName))
        case _ => List()
      })
      constructorParameters
    }

    def getConstructorParametersInParent(vc_name: Name, parent: TypeName) = {
      val factorySym = computeType(tq"${parent.toTypeName}").member(factoryName(vc_name).toTermName)
      val res = if (factorySym == NoSymbol)
        List()
      else {
        factorySym.asTerm.alternatives.map(alt => if (alt.isMethod) alt.asMethod.paramss.head.map {
          s => (s.name.toTermName, s.typeSignature.typeSymbol.name.toTypeName)
        }
        else List())
      }
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

    def transformBody(body: List[Tree], enclName: TypeName, parents: List[Tree]): List[Tree] = {
      val vcc = new VCContext(enclName, parents.map(p => getNameFromTree(p).toTypeName), body)

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

            val constructorParametersWithEmpty = ((if (!isAbstract(mods)) List(getConstructorParameters(vc_body)) else List()) ++ parents.flatMap(p => getConstructorParametersInParent(name, getNameFromTree(p).toTypeName))).distinct
            val constructorParametersWithoutEmpty = constructorParametersWithEmpty.filter(cp => !cp.isEmpty)
            val constructorParameters = if (constructorParametersWithoutEmpty.isEmpty) constructorParametersWithEmpty else constructorParametersWithoutEmpty

            val longest: List[(TermName, TypeName)] = parents.flatMap(p => getLongestConstructorParametersInParent(name, getNameFromTree(p).toTypeName)).distinct

            if (isOverridenVirtualClass(mods) && !getConstructorParameters(vc_body).isEmpty && !longest.zip(getConstructorParameters(vc_body)).map(a => a._1._1 == a._2._1 && a._1._2 == a._2._2).foldRight(true)((a, b) => a && b))
              c.abort(cd.pos, "Overriden virtual classes may only add constructor parameters at the end.\nThe constructor parameters have to start with:\n" +
                longest.map(cp => s"${cp._1}: ${cp._2}").mkString(", "))

            val inheritRel = vcc.getTypeBounds(name)
            val classInner = vcc.getClassMixins(name)
            val inheritRelMapped = mapInheritanceRelation(inheritRel, body)
            val typeDefInner: c.universe.Tree =
              typeTree(inheritRelMapped)

            val classTmpl = convertToTraitConstructor(vcc, impl, name, tparams, mods, classInner)

            val list = constructorParameters.flatMap { cp =>
              val vparamss = List(
                cp.map { case (name, tpe) => ValDef(Modifiers(PARAM), name, Ident(tpe), EmptyTree) })
              List[Tree](DefDef(Modifiers(DEFERRED), factoryName(name).toTermName, tparams, vparamss, getTypeApplied(name, body), EmptyTree))
            }

            List(
              TypeDef(Modifiers(DEFERRED), name, tparams, TypeBoundsTree(Select(Select(Ident(nme.ROOTPKG), newTermName("scala")), newTypeName("Null")), typeDefInner)),
              ClassDef(Modifiers(ABSTRACT | TRAIT), virtualTraitName(name, enclName).toTypeName, tparams, classTmpl)) ++ list

          //if (vcc.allBaseClasses.exists(p => parentContainsVirtualClass(Ident(p), name)) || (isAbstract(mods)))
          //  b
          //else
          //  DefDef(Modifiers(DEFERRED), factoryName(name).toTermName, tparams, vparamss, getTypeApplied(name, body), EmptyTree) ::
          //    b
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

    /**
     * we converted the class to a trait so change the constructor from <init> to $init$ and remove the super call...
     */
    def convertToTraitConstructor(vcc: VCContext, templ: c.universe.Template, name: TypeName, tparams: List[TypeDef], mods: Modifiers, classInner: List[TypeName]): c.universe.Template = {
      templ match {
        case Template(vc_parents, self, body) =>
          Template(tq"""scala.AnyRef""" :: classInner.filter(cn => cn != virtualTraitName(name, vcc.enclName)).map(cn => Ident(cn)), ValDef(Modifiers(PRIVATE), newTermName("self"), getTypeApplied(name, vcc.bodies), EmptyTree), transformVCBody(vcc, body, vc_parents, name, mods))
      }
    }

    def transformVCBody(vcc: VCContext, body: List[Tree], vc_parents: List[Tree], name: TypeName, mods: Modifiers) = {
      val constructorTransformed = body.flatMap(d => d match {
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) if (name.toString == "<init>") =>
          List(noParameterTraitConstructor) ++ vparamss.head.map(p => p match {
            case ValDef(mods, name, tparams, rhs) => {
              val newMods = if (mods.hasFlag(PARAMACCESSOR) && rhs == EmptyTree) Modifiers(DEFERRED) else NoMods
              ValDef(newMods, name, tparams, rhs)
            }
          })
        case ValDef(mods, name, tparams, rhs) if (mods.hasFlag(PARAMACCESSOR)) => List() //List(ValDef(Modifiers(DEFERRED), name, tparams, EmptyTree))
        case _ => List(d)
      })

      constructorTransformed
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

    def finalClass(enclName: TypeName, body: List[c.universe.Tree], parents: List[Tree]) = {
      val vcc = new VCContext(enclName, parents.map(p => getNameFromTree(p).toTypeName), body)

      val finalClassBody: List[c.universe.Tree] = noParameterConstructor :: body.flatMap(b =>
        b match {
          case ClassDef(mods, name, tparams, Template(vc_parents, _, vc_body)) if (isVirtualClass(mods)) =>
            val clasLin = vcc.getVirtualClassLinearization(name).reverse
            val classInner = vcc.getClassMixins(name)

            val typeDefInner = typeTree(vcc.getTypeBounds(name).map(Ident(_))) //typeTree(mapInheritanceRelation(getInheritanceRelation(body, vc_parents, name, parents, enclName), body))

            val classParents = mapInheritanceRelation(classInner.distinct, body)

            val constructorParametersWithEmpty = (List(getConstructorParameters(vc_body)) ++ parents.flatMap(p => getConstructorParametersInParent(name, getNameFromTree(p).toTypeName))).distinct
            val constructorParametersWithoutEmpty = constructorParametersWithEmpty.filter(cp => !cp.isEmpty)
            val constructorParameters = if (constructorParametersWithoutEmpty.isEmpty) constructorParametersWithEmpty else constructorParametersWithoutEmpty

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

        val constructorParametersWithEmpty = parents.flatMap(p => getConstructorParametersInParent(name, getNameFromTree(p).toTypeName)).distinct
        val constructorParametersWithoutEmpty = constructorParametersWithEmpty.filter(cp => !cp.isEmpty)
        val constructorParameters = if (constructorParametersWithoutEmpty.isEmpty) constructorParametersWithEmpty else constructorParametersWithoutEmpty

        makeFinalVirtualClassPart(name.toTypeName, enclName, mods, typeTree(typeDef), List(), classParents, constructorParameters)
      }
      val tmpl = Template(List(Ident(enclName)), emptyValDef, finalClassBody ++ bodyCompletion)

      ClassDef(Modifiers(), finalClassName(enclName).toTypeName, List(), tmpl)
    }

    case class VCContext(val enclName: TypeName, val parents: List[TypeName], val bodies: List[Tree]) {
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
        bodies.exists(b => b match {
          case ClassDef(mods, n, _, _) => (isVirtualClass(mods) || isOverridenVirtualClass(mods)) && n == name
          case _ => false
        })
      }

      def isVC(name: TypeName) = {
        isVCInBodies(name) || allBaseClasses.exists(p => parentContainsVirtualClass(Ident(p), name))
      }

      def findClassInBodies(name: TypeName) = {
        bodies.find(b => b match {
          case ClassDef(_, n, _, _) => name == n
          case _ => false
        })
      }

      def getTypeBounds(name: TypeName) = {
        (getVCParents(name) ++
          getVCTraits(name).reverse).distinct
      }

      def getParentsInParents(name: TypeName) = {
        this.parents.flatMap(p => getParentsInParent(p.toTypeName, name))
      }

      def getVCParents(name: TypeName) = {
        (getBaseClassesInBodies(name).filter(p => p != newTypeName("scala.AnyRef") && p != newTypeName("Object")) ++ getParentsInParents(name)).distinct
      }

      lazy val allBaseClasses: List[TypeName] = parents.flatMap(p => { computeType(Ident(p)).baseClasses.filter(n => !List("scala.AnyRef", "Any", "Object").contains(n.toString)).map(_.name.toTypeName) }).reverse.distinct.reverse

      lazy val toCompleteFromParents: List[TypeName] = parents.flatMap(p => getParentVCClasses(p).map(_.toTypeName)).filter(n => !findClassInBodies(n.toTypeName).isDefined).distinct

      def getBaseClassesInBodies(name: TypeName): List[TypeName] = {
        bodies.flatMap { b =>
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

    }

    // main transformation happens here
    val result: c.Tree = {
      annottees.map(_.tree).toList match {
        case (cd @ ClassDef(mods, name, tparams, Template(parents, self, body))) :: rest =>
          val classDef =
            q"""abstract trait $name[..$tparams] 
          		  extends ..$parents { outer => 
          		  ..${transformBody(body, name, parents)} 
                }"""
          val newObjectDef =
            q"""object ${name.toTermName} { 
          		def apply() = new ${finalClassName(name).toTypeName}; 
          		${finalClass(name, body, parents)} 
          	}"""

          q"{ ..${List(classDef, newObjectDef)} }"
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