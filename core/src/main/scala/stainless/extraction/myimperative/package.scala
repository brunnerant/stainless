/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package extraction

import scala.language.existentials

package object myimperative {

  type Trees = imperative.Trees

  object trees extends myimperative.Trees with oo.ClassSymbols {
    case class Symbols(
      functions: Map[Identifier, FunDef],
      sorts: Map[Identifier, ADTSort],
      classes: Map[Identifier, ClassDef],
      typeDefs: Map[Identifier, TypeDef],
    ) extends ClassSymbols with AbstractSymbols

    object printer extends Printer { val trees: myimperative.trees.type = myimperative.trees }
  }

  def extractor(implicit ctx: inox.Context) = {
    utils.DebugPipeline("Test", Test(trees, oo.trees))
  }

  def fullExtractor(implicit ctx: inox.Context) = extractor andThen nextExtractor
  def nextExtractor(implicit ctx: inox.Context) = oo.fullExtractor

  def phaseSemantics(implicit ctx: inox.Context): inox.SemanticsProvider { val trees: myimperative.trees.type } = {
    extraction.phaseSemantics(myimperative.trees)(fullExtractor)
  }

  def nextPhaseSemantics(implicit ctx: inox.Context): inox.SemanticsProvider { val trees: oo.trees.type } = {
    oo.phaseSemantics
  }
}
