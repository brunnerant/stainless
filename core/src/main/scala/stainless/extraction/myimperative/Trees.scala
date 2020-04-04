
package stainless
package extraction
package myimperative

trait Trees extends imperative.Trees { self =>

  /** Represents the shared referencing of an expression */
  case class Ref(expr: Expr) extends Expr with CachingTyped {
    protected def computeType(implicit s: Symbols): Type =
      unveilUntyped(RefType(expr))
  }

  /** Represents the mutable referencing of an expression */
  case class RefMut(expr: Expr) extends Expr with CachingTyped {
    protected def computeType(implicit s: Symbols): Type =
      unveilUntyped(RefMutType(expr))
  }

  /** Represents the dereferencing of an expression */
  case class Deref(expr: Expr) extends Expr with CachingTyped {
    protected def computeType(implicit s: Symbols): Type = expr.getType match {
      case RefType(tpe) => tpe
      case RefMutType(tpe) => tpe
      case _ => Untyped
    }
  }

  /** Represents the types of shared references */
  case class RefType(tpe: Type) extends Type

  /** Represents the types of mutable references */
  case class RefMutType(tpe: Type) extends Type

  override def getDeconstructor(that: inox.ast.Trees): inox.ast.TreeDeconstructor { val s: self.type; val t: that.type } = that match {
    case tree: Trees => new TreeDeconstructor {
      protected val s: self.type = self
      protected val t: tree.type = tree
    }.asInstanceOf[TreeDeconstructor { val s: self.type; val t: that.type }]

    case _ =>
      super.getDeconstructor(that)
  }
}

trait Printer extends imperative.Printer {
  protected val trees: Trees
  import trees._

  override protected def ppBody(tree: Tree)(implicit ctx: PrinterContext): Unit = tree match {
    case Ref(e) =>
      p"$e.ref"

    case RefMut(e) =>
      p"$e.refMut"

    case Deref(e) =>
      p"$e.deref"

    case RefType(t) =>
      p"Ref[$t]"

    case RefMutType(t) =>
      p"RefMut[$t]"

    case _ => super.ppBody(tree)
  }
}

trait TreeDeconstructor extends imperative.TreeDeconstructor {
  protected val s: Trees
  protected val t: Trees

  override def deconstruct(e: s.Expr): Deconstructed[t.Expr] = e match {
    case s.Ref(expr) =>
      (Seq(), Seq(), Seq(expr), Seq(), Seq(), (_, _, es, _, _) => t.Ref(es(0)))

    case s.RefMut(expr) =>
      (Seq(), Seq(), Seq(expr), Seq(), Seq(), (_, _, es, _, _) => t.RefMut(es(0)))

    case s.Deref(expr) =>
      (Seq(), Seq(), Seq(expr), Seq(), Seq(), (_, _, es, _, _) => t.Deref(es(0)))

    case _ =>
      super.deconstruct(e)
  }

  override def deconstruct(tpe: s.Type): Deconstructed[t.Type] = tpe match {
    case s.RefType(inner) =>
      (Seq(), Seq(), Seq(), Seq(inner), Seq(), (_, _, _, ts, _) => t.RefType(ts(0)))

    case s.RefMutType(inner) =>
      (Seq(), Seq(), Seq(), Seq(inner), Seq(), (_, _, _, ts, _) => t.RefMutType(ts(0)))

    case _ =>
      super.deconstruct(tpe)
  }
}
