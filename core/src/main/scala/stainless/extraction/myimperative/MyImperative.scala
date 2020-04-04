
package stainless
package extraction
package myimperative

trait MyImperative
  extends oo.CachingPhase
    with extraction.IdentitySorts
    with extraction.SimpleFunctions
    with extraction.SimplyCachedFunctions
    with oo.IdentityTypeDefs
    with oo.IdentityClasses { self =>

  val s: Trees
  val t: oo.Trees
      
  override protected def getContext(symbols: s.Symbols) = new TransformerContext(symbols)

  protected class TransformerContext(val symbols: s.Symbols) extends oo.TreeTransformer {
    val s: self.s.type = self.s
    val t: self.t.type = self.t

    import s._

    override def transform(expr: Expr): t.Expr = expr match {
      case Ref(e) =>
        println("visiting ref expression")
        transform(e)
      case RefMut(e) =>
        println("visiting ref mut expression")
        transform(e)
      case Deref(e) =>
        println("visiting deref expression")
        transform(e)
      case e =>
        super.transform(e)
    }

    override def transform(tpe: Type): t.Type = tpe match {
      case RefType(tpe) =>
        println(f"visiting ref type")
        transform(tpe)
      case RefMutType(tpe) =>
        println("visiting ref mut type")
        transform(tpe)
      case tpe =>
        super.transform(tpe)
    }

    override def transform(fd: FunDef): t.FunDef = {
      println(f"visiting $fd")
      def isMutableParam(vd: ValDef): Boolean = vd.tpe match {
        case RefMutType(_) => true
        case _ => false
      }

      // We retrieve the mutable parameter types
      val mutParamTypes = fd.params.map(_.tpe).collect {
        case RefMutType(tpe) => transform(tpe)
      }

      // The return type of the function now encapsulates the modified parameters
      val newRetType = t.tupleTypeWrap(transform(fd.returnType) +: mutParamTypes)

      // For each mutable parameter, we create a local variable
      val mutableParams = fd.params.filter(isMutableParam)
      val newBody = mutableParams.foldRight(fd.fullBody) { case (vd, newBody) =>
        s.LetVar(vd.freshen, vd.toVariable, newBody)
      }

      println(newBody)

      new t.FunDef(
        fd.id,
        fd.tparams.map(transform(_)),
        fd.params.map(transform(_)),
        newRetType,
        // newBody.asInstanceOf[t.Expr],
        transform(fd.fullBody),
        fd.flags.map(transform(_))
      )
    }
  }

  override protected def extractFunction(context: TransformerContext, fd: s.FunDef): t.FunDef = {
    context.transform(fd)
  }
}

object MyImperative {
  def apply(ts: Trees, tt: oo.Trees)(implicit ctx: inox.Context): ExtractionPipeline {
    val s: ts.type
    val t: tt.type
  } = new MyImperative {
    override val s: ts.type = ts
    override val t: tt.type = tt
    override val context = ctx
  }
}
