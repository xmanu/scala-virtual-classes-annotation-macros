import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.collection.mutable.HashMap

object virtualContext {

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._

    implicit class SymbolHelper(s: c.universe.Symbol) {
      def isDeferred: Boolean = {
        s.asInstanceOf[scala.reflect.internal.Symbols#Symbol].hasFlag(scala.reflect.internal.Flags.DEFERRED)
      }
    }

    // introduce paramaccessor as flag because macros don't provide it.
    val PARAMACCESSOR = (1 << 29).toLong.asInstanceOf[FlagSet]

    case class VCContext(val enclName: TypeName, val parents: List[TypeName], val bodies: List[Tree])

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

    def parameterConstructor(params: List[(TermName, TypeName)]) = {
      DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(params.map { case (name, tpe) => ValDef(Modifiers(PARAM | PARAMACCESSOR), name, Ident(tpe), EmptyTree) }), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(()))))
    }
    
    def isVirtualClass(mods: c.universe.Modifiers) = {
      mods.annotations.foldRight(false)((a, b) => b || (a.toString == "new virtual()" || a.toString == "new virtualOverride()"))
    }

    def isOverridenVirtualClass(mods: c.universe.Modifiers) = {
      mods.annotations.foldRight(false)((a, b) => b || (a.toString == "new virtualOverride()"))
    }

    def parentIsVirtualClass(parent: Tree, virtualClass: TypeName) = {
      computeType(parent).members.exists(s => s.name.decoded.startsWith(virtualClass.toString) && s.isType)
    }

    def isVC(name: TypeName)(implicit vcc: VCContext) = {
      vcc.bodies.exists(b => b match {
        case ClassDef(mods, _, _, _) => isVirtualClass(mods) || isOverridenVirtualClass(mods)
        case _ => false
      }) || vcc.parents.exists(p => parentIsVirtualClass(Ident(p), name))
    }

    def findInBodies(name: TypeName)(implicit vcc: VCContext) = {
      vcc.bodies.find(b => b match {
        case ClassDef(_, n, _, _) => name == n
        case _ => false
      })
    }

    def getParentsInParent(parent: TypeName, name: TypeName) = {
      val hi = computeType(Ident(parent)).member(name).typeSignature.asInstanceOf[scala.reflect.internal.Types#Type].bounds.hi
      val tpe_parents = hi.parents.map(_.typeSymbol.name.toTypeName.asInstanceOf[c.universe.TypeName])
      (if (tpe_parents.size == 1)
        List(hi.typeSymbol.name.toTypeName.asInstanceOf[c.universe.TypeName])
      else
        tpe_parents)
        .filter(!_.toString.startsWith("VC_TRAIT$"))
        .filter(p => p != newTypeName("scala.AnyRef") && p != newTypeName("Object"))
    }

    def getParentsInParents(name: TypeName)(implicit vcc: VCContext) = {
      vcc.parents.flatMap(p => getParentsInParent(p, name))
    }

    def getTraitParentsInParent(parent: TypeName, name: TypeName) = {
      val hi = computeType(Ident(parent)).member(name).typeSignature.asInstanceOf[scala.reflect.internal.Types#Type].bounds.hi
      val tpe_parents = hi.parents.map(_.typeSymbol.name.toTypeName.asInstanceOf[c.universe.TypeName])
      (if (tpe_parents.size == 1)
        List(hi.typeSymbol.name.toTypeName.asInstanceOf[c.universe.TypeName])
      else
        tpe_parents)
        .filter(_.toString.startsWith("VC_TRAIT$"))
    }

    def getTraitParentsInParents(name: TypeName)(implicit vcc: VCContext) = {
      vcc.parents.flatMap(p => getTraitParentsInParent(p, name))
    }

    def getVCParents(name: TypeName, parents: List[TypeName])(implicit vcc: VCContext) = {
      (parents.filter(p => p != newTypeName("scala.AnyRef") && p != newTypeName("Object")) ++ getParentsInParents(name)(vcc)).distinct
    }

    def currentPart(name: TypeName)(implicit vcc: VCContext) = {
      getTraitParentsInParents(name) ++
        (if (findInBodies(name).isDefined)
          List(newTypeName(virtualTraitName(name, vcc.enclName)))
        else
          List())
    }

    def getTypeBounds(name: TypeName, parents: List[TypeName])(implicit vcc: VCContext) = {
      (getVCParents(name, parents)(vcc) ++
        currentPart(name)).distinct
    }

    def getClassMixinsInParent(name: TypeName, parent: TypeName) = {
      val tpt = Select(Ident(parent.toString), newTypeName(finalClassName(parent.toString)))
      val tp = computeType(tpt)
      val fixClassTp = tp.declaration(newTypeName(fixClassName(name, parent)))
      if (tp != NoType && tp.baseClasses.length > 0 && fixClassTp.isClass) {
        fixClassTp.asClass.baseClasses.drop(1).dropRight(2).map(bc => bc.name.toTypeName)
      } else
        Nil
    }

    def getClassMixins(name: TypeName, parents: List[TypeName])(implicit vcc: VCContext): List[TypeName] = {
      (getVCParents(name, parents)(vcc).flatMap { n =>
        (findInBodies(n) match {
          case Some(b) => b match {
            case ClassDef(mods, _, _, Template(parents, _, _)) =>
              // we have defined this parent in this context so recurse...
              if (isVC(n))
                getClassMixins(n, parents.map(t => newTypeName(getNameFromTree(t))))(vcc)
              else
                List(n)
          }
          case None =>
            // we haven't defined this parent in this context so just find the right mixins in the fix class in our virtualContext's parents...
            List()
        }) ++ vcc.parents.flatMap(getClassMixinsInParent(n, _))
      } ++ currentPart(name)).distinct.reverse
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

    def transformVCBody(body: List[Tree], vc_parents: List[Tree], bodies: List[Tree], name: TypeName, mods: Modifiers, parents: List[TypeName], enclName: TypeName, inheritRel: List[String]) = {
      val constructorTransformed = body.flatMap(d => d match {
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) if (name.toString == "<init>") =>
          List(noParameterTraitConstructor)
        case ValDef(mods, name, tparams, rhs) if (mods.hasFlag(PARAMACCESSOR)) => List(ValDef(Modifiers(DEFERRED), name, tparams, EmptyTree))
        case _ => List(d)
      })

      val volatileFixesIntro: List[Tree] = {
        if (mods.hasFlag(ABSTRACT) && !parents.exists(p => parentIsVirtualClass(Ident(p), name)))
          List({
            val volatileFixName = newTermName(volatileFixMap.get(name.toString).get)
            DefDef(Modifiers(DEFERRED), volatileFixName, List(), List(List()), tq"""Int""", EmptyTree)
          })
        else List()
      }

      val volatileFixes: List[Tree] = {
        vc_parents.flatMap(p => {
          if (volatileFixMap.isDefinedAt(getNameFromSub(getNameFromTree(p)))) {
            val volatileFixName = newTermName(volatileFixMap.get(getNameFromSub(getNameFromTree(p))).get)
            List(DefDef(Modifiers(), volatileFixName, List(), List(List()), tq"""Int""", Literal(Constant(0))))
          } else Nil
        })
      }

      val volatileFixesInParents: List[Tree] = {
        val relevantInheritRel = inheritRel.filter(s => getParentNameFromSub(s) != enclName.toString && vc_parents.exists(t => getNameFromTree(t) == getNameFromSub(s)))
        val vfnip = relevantInheritRel.flatMap(s => volatileFixesInVCTrait(newTypeName(s))) //getVolatileFixNameInParents(inheritRel.map(newTypeName(_)), enclName, parents.map(_.toString))
        if (vfnip.length > 0)
          List({
            val volatileFixName = newTypeName(vfnip.head)
            DefDef(Modifiers(), volatileFixName, List(), List(List()), tq"""Int""", Literal(Constant(0)))
          })
        else
          List()
      }

      constructorTransformed ++ volatileFixesIntro ++ volatileFixes ++ volatileFixesInParents
    }

    /**
     * we converted the class to a trait so change the constructor from <init> to $init$ and remove the super call...
     */
    def convertToTraitConstructor(templ: c.universe.Template, name: TypeName, tparams: List[TypeDef], bodies: List[Tree], mods: Modifiers, parents: List[TypeName], enclName: TypeName, classInner: List[String]): c.universe.Template = {
      templ match {
        case Template(vc_parents, self, body) =>
          Template(classInner.filter(s => s != virtualTraitName(getNameFromSub(s), enclName)).map(s => Ident(newTypeName(s))), ValDef(Modifiers(PRIVATE), newTermName("self"), getTypeApplied(name, bodies), EmptyTree), transformVCBody(body, vc_parents, bodies, name, mods, parents, enclName, classInner))
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
        res
      } catch {
        case e: Throwable => e.printStackTrace(); false
      }
    }

    def getTypeParams(name: TypeName, bodies: List[Tree]) = {
      val res = bodies.map(b => b match {
        case ClassDef(_, n, tparams, impl) if (n.toString == getNameFromSub(name.toString)) =>
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
      val typeParams = getTypeParams(getNameFromSub(name.toString), bodies)
      if (typeParams.isEmpty || typeParams.get.isEmpty)
        Ident(name)
      else
        AppliedTypeTree(Ident(name), getTypeNames(typeParams.get).map(t => Ident(t)))
    }

    def mapInheritanceRelation(inheritRel: List[String], bodies: List[Tree]) = {
      val inheritRelMapped = inheritRel.map(s =>
        getTypeApplied(s, bodies))
      inheritRelMapped
    }

    def declsInVCTrait(traitName: TypeName): List[String] = {
      getVCClassesSymbols(getParentNameFromSub(traitName.toString))
        .filter(s => s.isClass && s.name == traitName)
        .flatMap(s =>
          s.asClass.toType.declarations
            .filter(s => s.isMethod && !s.asMethod.isConstructor && !s.isDeferred)
            .map(_.name.toString)
            .filter(s => !s.endsWith("_$eq")))
        .map(_.trim).distinct
    }

    def volatileFixesInVCTrait(traitName: TypeName): List[String] = {
      getVCClassesSymbols(getParentNameFromSub(traitName.toString))
        .filter(s => s.isClass && s.name == traitName)
        .flatMap(s =>
          s.asClass.toType.declarations
            .filter(s => s.isMethod && !s.asMethod.isConstructor && s.isDeferred)
            .map(_.name.toString)
            .filter(s => s.startsWith("volatileFix$")))
        .map(_.trim).distinct
    }

    def membersOf(cls: TypeName, bodies: List[Tree], noAbstract: Boolean = false): List[String] = {
      bodies.flatMap(
        t => t match {
          case cd @ ClassDef(mods, name, tparams, Template(parents, valDef, body)) if (isVirtualClass(mods) && name == cls) =>
            body.flatMap(b => b match {
              case dd @ DefDef(mods, defName, tparams, vparamss, tpt, rhs) if (defName != nme.CONSTRUCTOR && (!noAbstract || !mods.hasFlag(DEFERRED))) => {
                List(defName.toString)
              }
              case _ => List()
            })
          case _ => List()
        })
    }

    def nameClashesForVCClass(vc: TypeName, classParents: List[Tree], enclName: TypeName, bodies: List[Tree]): Map[String, TypeName] = {
      var result = Map[String, TypeName]()
      var seen = List[String]()

      val classes = (virtualTraitName(vc, enclName) :: classParents.map(getNameFromTree(_))).reverse.distinct

      classes.foreach {
        p =>
          val p_parent = getParentNameFromSub(p)
          val members_in_p = if (p_parent != enclName.toString) {
            declsInVCTrait(p)
          } else {
            membersOf(getNameFromSub(p), bodies, true)
          }
          members_in_p.foreach { m =>
            if (!seen.contains(m))
              seen ::= m
            else {
              result += m -> newTypeName(p)
            }
          }
      }
      result
    }

    def getConstructorParameters(vc_body: List[Tree]) = {
      val constructorParameters = vc_body.flatMap(b => b match {
        case ValDef(mods, name, Ident(tn), _) if mods.hasFlag(PARAMACCESSOR) => List((name, tn.toTypeName))
        case _ => List()
      })
      constructorParameters
    }
    
    def getConstructorParametersInParent(vc_name: TypeName, parent: TypeName) = {
      val factorySym = computeType(Ident(parent)).member(newTermName(factoryName(vc_name)))
      if (factorySym == NoSymbol)
        List()
      else {
        val params = factorySym.asMethod.paramss.head
        params.map {
          s => (s.name.toTermName, s.typeSignature.typeSymbol.name.toTypeName)
        }
      }
    }

    def transformBody(body: List[Tree], enclName: TypeName, parents: List[Tree]): List[Tree] = {
      implicit val vcc = new VCContext(enclName, parents.map(p => newTypeName(getNameFromTree(p))), body)

      body.foreach(b =>
        b match {
          case cd @ ClassDef(mods, name, tparams, impl) if (isVirtualClass(mods) && mods.hasFlag(ABSTRACT)) =>
            volatileFixMap.put(name.toString, c.fresh("volatileFix$"))
          case _ => ;
        })

      val bodyTransform = body.flatMap(b =>
        b match {
          case cd @ ClassDef(mods, name, tparams, impl) if (isVirtualClass(mods)) =>
            if (mods.hasFlag(TRAIT))
              c.error(cd.pos, "Only classes can be declared as virtual (they will be converted to traits though).")
            //TODO: support type parameters
            if (!tparams.isEmpty)
              c.error(cd.pos, "Type parameters are currently not supported.")

            if (!isOverridenVirtualClass(mods) && parents.exists(t => parentIsVirtualClass(t, name)))
              c.error(cd.pos, s"The following parents of the class family already declare a class with the name $name: " + parents.filter(t => parentIsVirtualClass(t, name)).mkString(", ") + "\nTo override functionality, declare the virtual class @virtualOverride.")

            val Template(vc_parents, _, vc_body) = impl
            
            if (isOverridenVirtualClass(mods) && !getConstructorParameters(vc_body).isEmpty)
              c.error(cd.pos, "Overriden virtual classes cannot define constructor parameters.")
              

            val inheritRel = getTypeBounds(name, vc_parents.map(p => newTypeName(getNameFromTree(p)))).map(_.toString)
            val classInner = getClassMixins(name, vc_parents.map(p => newTypeName(getNameFromTree(p)))).map(_.toString)
            val inheritRelMapped = mapInheritanceRelation(inheritRel, body)
            val typeDefInner: c.universe.Tree = typeTree(inheritRelMapped)

            val classTmpl = convertToTraitConstructor(impl, name, tparams, body, mods, parents.map(p => newTypeName(getNameFromTree(p))), enclName, classInner)

            val constructorParameters = if (!isOverridenVirtualClass(mods)) 
              getConstructorParameters(vc_body)
            else
              parents.flatMap(p => getConstructorParametersInParent(name, getNameFromTree(p))).distinct

            val vparamss = List(
              constructorParameters.map { case (name, tpe) => ValDef(Modifiers(PARAM), name, Ident(tpe), EmptyTree) })

            //List(List(ValDef(Modifiers(PARAM), newTermName("value"), Ident(newTypeName("Int")), EmptyTree), ValDef(Modifiers(PARAM), newTermName("value2"), Ident(newTypeName("Int")), EmptyTree)))

            val b = List(
              TypeDef(Modifiers(DEFERRED), name, tparams, TypeBoundsTree(Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Null")), typeDefInner)),
              ClassDef(Modifiers(ABSTRACT | TRAIT), virtualTraitName(name, enclName), tparams, classTmpl))
            if (parentIsVirtualClass(parents(0), name) || (mods.hasFlag(ABSTRACT)))
              b
            else
              ModuleDef(Modifiers(), name.toTermName, Template(List(Select(Ident("scala"), newTypeName("AnyRef"))), emptyValDef, 
                  List(DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))), 
                      DefDef(Modifiers(), newTermName("apply"), List(), vparamss, TypeTree(), Apply(Ident(newTermName(factoryName(name))), constructorParameters.map { case (name, tpe) => Ident(name) })))
                      // TODO: implement unapply method
              )) ::
                DefDef(Modifiers(DEFERRED), newTermName(factoryName(name)), tparams, vparamss, getTypeApplied(name, body), EmptyTree) ::
                b
          case DefDef(mods, name, tparams, vparamss, tpt, rhs) if (name.toString == "<init>") =>
            List(noParameterTraitConstructor)
          case _ => List(b)
        })
      val toCompleteFromParents = parents.flatMap(p => getParentVCClasses(getNameFromTree(p))).filter(!finalClassBodyContainsVCClass(body, _)).distinct

      val bodyCompletion = toCompleteFromParents.map { name =>
        val typeDef = getTypeBounds(newTypeName(name), getParentsInParents(newTypeName(name)))

        TypeDef(Modifiers(DEFERRED), name, List(), TypeBoundsTree(Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Null")), typeTree(typeDef.map(Ident(_)))))
      }

      bodyTransform ++ bodyCompletion
    }

    def makeFinalVirtualClassPart(name: TypeName, enclName: TypeName, mods: Modifiers, typeDef: Tree, tparams: List[TypeDef], classParents: List[Tree], nameClashes: Map[String, TypeName], constructorParameters: List[(TermName, TypeName)]): List[Tree] = {
      val vparamss = List(
        constructorParameters.map { case (name, tpe) => ValDef(Modifiers(PARAM), name, Ident(tpe), EmptyTree) })

      val clashOverrides = nameClashes.map(s => DefDef(Modifiers(OVERRIDE), newTermName(s._1), List(), List(), TypeTree(), Select(Super(This(tpnme.EMPTY), s._2), newTermName(s._1))))

      val fcn = newTypeName(fixClassName(name, enclName))
      
      val fL = List(TypeDef(Modifiers(), name, tparams, typeDef),
          //q"$mods class $fcn[..$tparams] (...$vparamss) extends ..$classParents { ..$clashOverrides }")
        ClassDef(mods, fixClassName(name, enclName), tparams, Template(classParents, emptyValDef, constructorParameters.map { case (name, tpe) => ValDef(Modifiers(PARAMACCESSOR), name, Ident(tpe), EmptyTree) } ++ List(parameterConstructor(constructorParameters)) ++ clashOverrides)))

      val fixClassTypeName = if (tparams.isEmpty)
        Ident(newTypeName(fixClassName(name, enclName)))
      else
        AppliedTypeTree(Ident(newTypeName(fixClassName(name, enclName))), getTypeNames(tparams).map(t => Ident(t)))

      if (!(mods.hasFlag(ABSTRACT)))
        DefDef(Modifiers(), newTermName(factoryName(name)), tparams, vparamss, TypeTree(), Apply(Select(New(fixClassTypeName), nme.CONSTRUCTOR), constructorParameters.map {case (name, tpe) => Ident(name)} )) ::
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
      implicit val vcc = new VCContext(enclName, parents.map(p => newTypeName(getNameFromTree(p))), body)

      val finalClassBody: List[c.universe.Tree] = noParameterConstructor :: body.flatMap(b =>
        b match {
          case ClassDef(mods, name, tparams, Template(vc_parents, _, vc_body)) if (isVirtualClass(mods)) =>
            val typeDefInner = typeTree(getTypeBounds(name, vc_parents.map(p => newTypeName(getNameFromTree(p)))).map(Ident(_))) //typeTree(mapInheritanceRelation(getInheritanceRelation(body, vc_parents, name, parents, enclName), body))

            val classInner = getClassMixins(name, vc_parents.map(p => newTypeName(getNameFromTree(p)))).map(_.toString) //getInheritanceTreeComplete(body, name, enclName, parents)

            val classAdditions = classInner.flatMap(p => if (!classInner.contains(virtualTraitName(getNameFromSub(p.toString), enclName)) &&
              finalClassBodyContainsVCClass(body, getNameFromSub(p.toString)))
              List(virtualTraitName(getNameFromSub(p.toString), enclName))
            else
              List())

            val classParents = mapInheritanceRelation((classInner ++ classAdditions).distinct, body)

            val nc = nameClashesForVCClass(name, classParents, enclName, body)

            val constructorParameters = if (!isOverridenVirtualClass(mods)) 
              getConstructorParameters(vc_body)
            else
              parents.flatMap(p => getConstructorParametersInParent(name, getNameFromTree(p))).distinct
            
            makeFinalVirtualClassPart(name, enclName, mods, typeDefInner, tparams, classParents, nc, constructorParameters)

          case _ => Nil
        })

      val toCompleteFromParents = parents.flatMap(p => getParentVCClasses(getNameFromTree(p))).filter(!finalClassBodyContainsVCClass(finalClassBody, _)).distinct

      val bodyCompletion = toCompleteFromParents.flatMap { name =>
        val typeDef = getTypeBounds(newTypeName(name), getParentsInParents(newTypeName(name))).map(Ident(_))

        val classParents = getClassMixins(newTypeName(name), getParentsInParents(newTypeName(name))).map(Ident(_))

        val mods = if (parents.exists(p => parentContains(p.toString, factoryName(name))))
          Modifiers()
        else
          Modifiers(ABSTRACT)

        val nc = nameClashesForVCClass(name, classParents, enclName, body)
        makeFinalVirtualClassPart(name, enclName, mods, typeTree(typeDef), List(), classParents, nc, getConstructorParametersInParent(name, getNameFromTree(parents.filter(p => getParentVCClasses(getNameFromTree(p)).contains(name)).head)))
      }
      val tmpl = Template(List(Ident(enclName)), emptyValDef, finalClassBody ++ bodyCompletion)

      ClassDef(Modifiers(), finalClassName(enclName), List(), tmpl)
    }

    val result: c.Tree = {
      annottees.map(_.tree).toList match {
        case (cd @ ClassDef(mods, name, tparams, Template(parents, self, body))) :: rest =>
          val classDef = ClassDef(Modifiers(ABSTRACT | TRAIT), name, tparams, Template(parents, ValDef(Modifiers(PRIVATE), newTermName("outer"), TypeTree(), EmptyTree), transformBody(body, name, parents)))
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