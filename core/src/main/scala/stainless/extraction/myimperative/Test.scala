
package stainless
package extraction
package myimperative

trait Test
  extends oo.CachingPhase
    with extraction.IdentitySorts
    with extraction.IdentityFunctions
    with oo.IdentityTypeDefs
    with oo.IdentityClasses {
  override protected type TransformerContext = s.Symbols
  override protected def getContext(symbols: s.Symbols) = symbols
}

object Test {
  def apply(ts: Trees, tt: oo.Trees)(implicit ctx: inox.Context): ExtractionPipeline {
    val s: ts.type
    val t: tt.type
  } = new Test {
    override val s: ts.type = ts
    override val t: tt.type = tt
    override val context = ctx
  }
}
