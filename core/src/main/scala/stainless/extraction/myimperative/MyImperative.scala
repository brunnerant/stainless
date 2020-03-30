
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

  protected class TransformerContext(val symbols: s.Symbols) extends oo.TreeTransformer { // CheckingTransformer {
    val s: self.s.type = self.s
    val t: self.t.type = self.t

    override def transform(expr: s.Expr): t.Expr = expr match {
      case s.Ref(e) =>
        println("visiting ref expression")
        transform(e)
      case s.RefMut(e) =>
        println("visiting ref mut expression")
        transform(e)
      case s.Deref(e) =>
        println("visiting deref expression")
        transform(e)
      case e =>
        super.transform(e)
    }

    override def transform(tpe: s.Type): t.Type = tpe match {
      case s.RefType(tpe) =>
        println("visiting ref type")
        transform(tpe)
      case s.RefMutType(tpe) =>
        println("visiting ref mut type")
        transform(tpe)
      case tpe =>
        super.transform(tpe)
    }
  }

  override protected def extractFunction(context: TransformerContext, fd: s.FunDef): t.FunDef = {
    fd.asInstanceOf[t.FunDef]
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
