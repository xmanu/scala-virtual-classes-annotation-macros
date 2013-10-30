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

    def isVirtualClass(mods: c.universe.Modifiers) = {
      mods.annotations.foldRight(false)((a, b) => b || (a.toString == "new virtual()"))
    }

    def parentIsVirtualClass(parent: Tree, virtualClass: TypeName) = {
      val res = computeType(parent).members.exists(s => s.isClass && s.name.decoded.startsWith("VC_TRAIT$" + parent.toString + "$" + virtualClass.toString))
      //println("parentIsVirtualClass: parent: " + parent + "; virtualClass: " + virtualClass + " = " + res)
      res
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
     * TODO: re-insert the body...
     */
    def convertToTraitConstructor(templ: c.universe.Template, name: TypeName): c.universe.Template = {
      templ match {
        case Template(parents, self, body) =>
          Template(List(tq"""scala.AnyRef"""), ValDef(Modifiers(PRIVATE), newTermName("self"), Ident(name), EmptyTree), body.map(d => d match {
            case DefDef(mods, name, tparams, vparamss, tpt, rhs) if (name.toString == "<init>") =>
              DefDef(mods, newTermName("$init$"), tparams, vparamss, tpt, Block(List(), Literal(Constant(()))))
            case _ => d
          }))
      }
    }

    def typeTree(types: List[String]): c.universe.Tree = {
      if (types.length == 1)
        Ident(newTypeName(types(0)))
      else
        CompoundTypeTree(Template(types.map(s => Ident(newTypeName(s))), emptyValDef, List()))
    }

    def getNameFromSub(name: String) = name.takeRight(name.length - name.lastIndexOf("$") - 1)
    
    def getInheritanceRelation(bodies: List[c.universe.Tree], impl: Tree, name: TypeName, parent: Tree, enclName: TypeName): List[String] = {
      val Template(parents, valDef, body) = impl

      // family inheritance
      val family = List(virtualTraitName(name, enclName))

      //println("parents: " + parents.mkString(" "))
      // are any parents from the top family virtualclasses?
      //val parentInheritance = parents.filter(p => parentIsVirtualClass(parent, p.toString)).map(_.toString)

      val parentInheritance = if (parentIsVirtualClass(parent, name)) {
        getInheritanceTreeInParents(name, parent.toString).reverse
      } else
        List()
        
      println("parentInheritance: " + parentInheritance.mkString(" "))
        
      val ownInheritance = parents.filter(p => bodies.exists(b => b match {
        case ClassDef(mods, name, tparams, impl) => (isVirtualClass(mods) && p.toString == name.toString)
        case _ => false
      })).map(_.toString)

      //println("body: " + body.mkString("; "))
      //println("ownInheritance: " + ownInheritance.mkString(" "))

      //val ownInheritanceReplacement: List[String] = if (parentIsVirtualClass(parent, name) && parentInheritance.length > 0) List(parentInheritance(0).take(parentInheritance(0).lastIndexOf("$"))) else List[String]()
      
      val all = ownInheritance ++ parentInheritance ++ family // TODO: What is right linearization
      //res
      
      //val classInnerParentsAdditions = all.flatMap(p => if (!all.contains(virtualTraitName(getNameFromSub(p.toString), enclName))) List(virtualTraitName(getNameFromSub(p.toString), enclName)) else List())
      println("all: " + all.mkString(" "))
      
      val res = if (all.length > 2 && getNameFromSub(all.tail.head) != name.toString)
        getNameFromSub(all.tail.head) :: all.head :: all.tail.tail
      else
          all
         
      println("res: " + res.mkString(" "))
          
      res
    }

    def getInheritanceTree(bodies: List[c.universe.Tree], className: TypeName): List[String] = {
      val res = bodies.flatMap(b => b match {
        case ClassDef(mods, name, tparams, Template(parents, valDef, body)) if (name == className) =>
          name.toString :: getInheritanceTree(bodies, parents(0).toString)
        case _ => List()
      })
      //println("getInheritanceTree: className: " + className.toString + "; " + res.mkString(" "))
      res
    }

    def getInheritanceTreeInParents(className: TypeName, parentName: TypeName): List[String] = {
      println("getInheritanceTreeInParents: className: " + className + "; parentName: " + parentName)
      try {
        val tpt = Select(Ident(parentName.toTermName), newTypeName(finalClassName(parentName)))
        //println("classInnerParents Tree: " + show(tpt))
        val tp = computeType(tpt)
        //println("classInnerParents decl: " + tp.declarations.mkString(" "))
        val fixClassTp = tp.declaration(newTypeName(fixClassName(className, parentName)))
        //println("classInnerParents fixClassTp: " + fixClassTp)
        if (tp != NoType && tp.baseClasses.length > 0) {
          //println("classInnerParents baseClasses: " + fixClassTp.asClass.baseClasses.mkString(" "))
          fixClassTp.asClass.baseClasses.drop(1).dropRight(2).map(bc => bc.name.toString)
        } else
          Nil
      } catch {
        case e: Throwable => e.printStackTrace(); Nil
      }
    }

    def transformBody(body: List[c.universe.Tree], enclName: TypeName, parent: Tree): List[c.universe.Tree] = {
      body.flatMap(b =>
        b match {
          case ClassDef(mods, name, tparams, impl) if (isVirtualClass(mods)) =>
            //println("getInheritance: " + getInheritanceRelation(body, impl, name, parent, enclName))
            val inheritRel = getInheritanceRelation(body, impl, name, parent, enclName)
            val typeDefInner: c.universe.Tree = typeTree(inheritRel.filter(s => !parentIsVirtualClass(parent, name) || inheritRel.length < 3 || s != virtualTraitName(name, enclName)))

            val b = List(
              TypeDef(Modifiers(DEFERRED), name, List(), TypeBoundsTree(Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Null")), typeDefInner)),
              ClassDef(Modifiers(ABSTRACT | DEFAULTPARAM), virtualTraitName(name, enclName), tparams, convertToTraitConstructor(impl, name)))
            if (parentIsVirtualClass(parent, name) || (mods.flags | ABSTRACT) == mods.flags)
              b
            else
              DefDef(Modifiers(DEFERRED), newTermName(factoryName(name)), List(), List(), Ident(newTypeName(virtualTraitName(name, enclName))), EmptyTree) :: b
          case _ => List(b)
        })
    }

    def finalClass(enclName: TypeName, body: List[c.universe.Tree], parent: Tree) = {
      val finalClassBody = noParameterConstructor :: body.flatMap(b =>
        b match {
          case ClassDef(mods, name, tparams, impl) if (isVirtualClass(mods)) =>
            val typeDefInner: c.universe.Tree = typeTree(getInheritanceRelation(body, impl, name, parent, enclName))

            val Template(parents, _, _) = impl

            val classInner = getInheritanceTree(body, name).map(s => Ident(newTypeName(virtualTraitName(s, enclName))))

            val classInnerParents = getInheritanceTreeInParents(name, parent.toString).map(s => Ident(newTypeName(s)))
            
            val classInnerParentsAdditions = classInnerParents.flatMap(p => if (!(classInnerParents ++ classInner).map(_.toString).contains(virtualTraitName(getNameFromSub(p.toString), enclName))) List(virtualTraitName(getNameFromSub(p.toString), enclName)) else List()).map(s => Ident(newTypeName(s)))

            println("classInner: " + classInner.mkString(" "))
            println("classInnerParents: " + classInnerParents.mkString(" "))

            val fL = List(TypeDef(Modifiers(), name, List(), typeDefInner),
              ClassDef(mods, fixClassName(name, enclName), List(), Template(classInner ++ classInnerParents ++ classInnerParentsAdditions, emptyValDef, List(noParameterConstructor))))

            if ((mods.flags | ABSTRACT) != mods.flags)
              ModuleDef(Modifiers(), name.toTermName, Template(List(Select(Ident("scala"), newTypeName("AnyRef"))), emptyValDef, List(DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))), DefDef(Modifiers(), newTermName("apply"), List(), List(List()), TypeTree(), Ident(newTermName(factoryName(name))))))) :: 
              DefDef(Modifiers(), newTermName(factoryName(name)), List(), List(), TypeTree(), Apply(Select(New(Ident(newTypeName(fixClassName(name, enclName)))), nme.CONSTRUCTOR), List())) :: 
              fL
            else
              fL

          case _ => Nil
        })

      val tmpl = Template(List(Ident(enclName)), emptyValDef, finalClassBody)

      ClassDef(Modifiers(), finalClassName(enclName), List(), tmpl)
    }

    val result: c.Tree = {
      annottees.map(_.tree).toList match {
        case (cd @ ClassDef(mods, name, tparams, Template(parents, self, body))) :: rest =>
          val classDef = ClassDef(Modifiers(ABSTRACT), name, tparams, Template(parents, self, transformBody(body, name, parents(0))))
          // def <init>() = super.<init>()
          //lazy val objectConstructor =
          //  q"""def ${nme.CONSTRUCTOR}() = { super.${nme.CONSTRUCTOR}(); () }"""
          def objectConstructor = DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(()))))
          val newObjectBody: List[Tree] = objectConstructor :: finalClass(name, body, parents(0)) :: DefDef(Modifiers(), newTermName("apply"), List(), List(List()), TypeTree(), Apply(Select(New(Ident(newTypeName(finalClassName(name)))), nme.CONSTRUCTOR), List())) :: Nil
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
  def macroTransform(annottees: Any*) = macro virtualMacro.impl
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