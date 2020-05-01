/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package extraction
package imperative

import inox.FatalError

trait EffectsAnalyzer extends oo.CachingPhase {
  val s: Trees
  import s._

  sealed abstract class Accessor
  case class ADTFieldAccessor(selector: Identifier) extends Accessor
  case class ClassFieldAccessor(selector: Identifier) extends Accessor
  case class ArrayAccessor(index: Expr) extends Accessor
  case class MutableMapAccessor(index: Expr) extends Accessor

  case class Path(path: Seq[Accessor]) {
    def :+(elem: Accessor): Path = Path(path :+ elem)
    def +:(elem: Accessor): Path = Path(elem +: path)
    def ++(that: Path): Path = Path(this.path ++ that.path)

    def toSeq: Seq[Accessor] = path

    def asString(implicit printerOpts: PrinterOptions): String =
      if (path.isEmpty) "<empty>"
      else path.map {
        case ADTFieldAccessor(id) => s".${id.asString}"
        case ClassFieldAccessor(id) => s".${id.asString}"
        case ArrayAccessor(idx) => s"(${idx.asString})"
        case MutableMapAccessor(idx) => s"(${idx.asString})"
      }.mkString("")

    override def toString: String = asString
  }

  object Path {
    def empty: Path = Path(Seq.empty)
  }

  /**
   * This represents the target of a mutation. It consists of a variable and a path to
   * the part of the variable that is mutated. It optionnally includes a condition if the
   * the assignment is conditionnal.
   */
  case class Target(receiver: Variable, condition: Option[Expr], path: Path) {
    def :+(elem: Accessor): Target = Target(receiver, condition, path :+ elem)

    def append(that: Target): Target = (condition, that.condition) match {
      case (condition, None) =>
        Target(receiver, condition, path ++ that.path)
      case (None, _) =>
        Target(receiver, that.condition, path ++ that.path)
      case (Some(condition), Some(thatCondition)) =>
        Target(receiver, Some(And(condition, thatCondition)), path ++ that.path)
    }

    def asString(implicit printerOpts: PrinterOptions): String =
      s"Target(${receiver.asString}, ${condition.map(_.asString)}, ${path.asString})"

    override def toString: String = asString
  }

  /**
   * This represents an effect that occurs on a variable. It consists of a target,
   * and the value assigned to it.
   */
  case class Effect(target: Target, value: Expr)

  /**
   * An environment contains a mapping between variables and targets. This allows to keep track of the
   * roots of all assignments. For example, if a variable is declared as such:
   * val y = x.<path>
   * then the environment will contain a pair y -> Target(x, None, <path>)
   * It also contains a map of all the local function defined in scope.
   */
  case class EffectsEnv(
    variables: Map[Variable, Set[Target]],
    locals: Map[Identifier, LocalFunDef]
  ) {
    // Adds or updates the targets that a variable points to
    def withVariable(v: Variable, targets: Set[Target]): EffectsEnv =
      copy(variables = variables + (v -> targets))

    // This version is useful for let bindings
    def withVariable(v: Variable, value: Expr)(implicit symbols: Symbols): EffectsEnv =
      withVariable(v, getTargets(value, this))

    // This is useful for adding variables that are roots (i.e. don't refer to anything else than themselves)
    def withRoots(vs: Set[ValDef]): EffectsEnv =
      copy(variables = variables ++ vs.map(v => v.toVariable -> Set(Target(v.toVariable, None, Path.empty))))

    // Adds local functions to the environment
    def withLocals(fds: Seq[LocalFunDef]): EffectsEnv =
      copy(locals = locals ++ fds.map(fd => fd.id -> fd))

    // This allows to propagate targets for variables that were already
    // declared in the environment. If the target variable is not in the
    // environment, an exception is thrown.
    def propagateTarget(t: Target): Set[Target] =
      variables.get(t.receiver)
        .map(ts => ts.map(_.append(t)))
        .getOrElse(throw MalformedStainlessCode(t.receiver, s"Unknown variable ${t.receiver}"))

    def +(mappings: Map[Variable, Set[Target]]): EffectsEnv = copy(variables = variables ++ mappings)
  }

  object EffectsEnv {
    def empty = EffectsEnv(Map.empty, Map.empty)
  }

  /**
   * This returns all the targets that would be mutated if the given expression was assigned to.
   * For example, given the expression x.y[0], the target would be Target(x, None, .y[0]).
   * If branching occurs, then several targets are returned.
   */
  def getTargets(expr: Expr, env: EffectsEnv)(implicit symbols: Symbols): Set[Target] = {
    def error(expr: Expr) =
      throw MalformedStainlessCode(expr, s"Couldn't compute targets in: $expr")

    def rec(expr: Expr, path: Seq[Accessor])(implicit env: EffectsEnv): Set[Target] = expr match {
      // Variables might refer to the environment, so we need to propagate them
      case v: Variable =>
        env.propagateTarget(Target(v, None, Path(path)))
      
      // Those are the ways a path can increase
      case ADTSelector(e, id) => rec(e, ADTFieldAccessor(id) +: path)
      case ClassSelector(e, id) => rec(e, ClassFieldAccessor(id) +: path)
      case ArraySelect(a, idx) => rec(a, ArrayAccessor(idx) +: path)
      case MutableMapApply(a, idx) => rec(a, MutableMapAccessor(idx) +: path)
      case MutableMapDuplicate(m) => rec(m, path)

      // Those are the ways a path can decrease
      case ADT(id, _, args) => path match {
        case ADTFieldAccessor(fid) +: rest =>
          rec(args(symbols.getConstructor(id).fields.indexWhere(_.id == fid)), rest)
        case _ =>
          error(expr)
      }

      case ClassConstructor(ct, args) => path match {
        case ClassFieldAccessor(fid) +: rest =>
          rec(args(ct.tcd.fields.indexWhere(_.id == fid)), rest)
        case _ =>
          error(expr)
      }

      // If-then-else's introduce conditionnal targets
      case IfExpr(cnd, thn, els) =>
        def conj(cnd1: Expr, cnd2: Option[Expr]): Expr = cnd2 map { cnd2 =>
          And(cnd1, cnd2.setPos(cnd1)).setPos(cnd1)
        } getOrElse(cnd1)

        rec(thn, path).map(t => Target(t.receiver, Some(conj(cnd, t.condition)), t.path)) ++
        rec(els, path).map(e => Target(e.receiver, Some(conj(Not(cnd).setPos(cnd), e.condition)), e.path))

      // Match expressions are converted to if-then-else for simplicity
      case m: MatchExpr =>
        rec(symbols.matchToIfThenElse(m), path)

      // If the function creates aliasing, we need to keep track of it
      case fi: FunctionInvocation if createsAliasing(fi.tfd.functionType) =>
        // If the function is recursive, there is no simple way of knowing what it could return
        if (symbols.isRecursive(fi.id)) error(expr)
        else exprOps.withoutSpecs(symbols.simplifyLets(fi.inlined))
          .map(rec(_, path))
          .getOrElse(error(expr))

      // Functions that don't create aliasing cannot create targets
      case fi: FunctionInvocation =>
        Set.empty

      // Lets add variables to the environment
      case Let(vd, e, b) =>
        val exprTargets = rec(e, Seq.empty)
        rec(b, path)(env + Map(vd.toVariable -> exprTargets))

      case LetVar(vd, e, b) =>
        val exprTargets = rec(e, Seq.empty)
        rec(b, path)(env + Map(vd.toVariable -> exprTargets))

      // The reference ASTs are only a mean to gather more information, they are aren't
      // semantically useful
      case Ref(e) => rec(e, path)
      case RefMut(e) => rec(e, path)
      case Deref(e) => rec(e, path)

      // For now this case allows to catch all unimplemented features nicely
      case _ => error(expr)
    }

    rec(expr, Seq.empty)(env)
  }

  protected def typeToAccessor(tpe: Type, id: Identifier)(implicit s: Symbols): Accessor = tpe match {
    case at: ADTType   => ADTFieldAccessor(id)
    case ct: ClassType => ClassFieldAccessor(id)
    case ta: TypeApply => typeToAccessor(ta.getType, id)
    case _ => throw FatalError(s"Cannot have accessors over type $tpe")
  }
  
  /**
   * This computes which parameters are passed by mutable reference.
   */
  def refMutParams(params: Seq[ValDef]): Seq[ValDef] = params.filter(_.tpe match {
    case _: RefMutType => true
    case _ => false
  })

  /**
   * This computes which parameters are passed by reference.
   */
  def refParams(params: Seq[ValDef]): Seq[ValDef] = params.filter(_.tpe match {
    case _: RefType => true
    case _ => false
  })

  /**
   * This computes which parameters are passed by value (i.e. not by reference).
   */
  def byValParams(params: Seq[ValDef]): Seq[ValDef] = params.filter(_.tpe match {
    case _: RefType => false
    case _: RefMutType => false
    case _ => true
  })

  /**
   * This type is used to split the types according to their categories
   * (by val, by reference, by mutable reference)
   */
  type Split[+T] = (Seq[T], Seq[T], Seq[T])

  /**
   * This function splits the params into their categories
   */
  def split(params: Seq[ValDef]): Split[ValDef] =
    (byValParams(params), refParams(params), refMutParams(params))

  /**
   * Returns whether the function can return a value that refers to some of its environment,
   * and that can be mutated. In our simple model, it is only the case if the function
   * returns a mutable reference.
   */
  def createsAliasing(ft: FunctionType): Boolean = ft.to match {
    case _: RefMutType => true
    case _ => false
  }
}
