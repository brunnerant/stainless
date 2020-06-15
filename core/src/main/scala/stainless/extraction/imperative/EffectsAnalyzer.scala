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
   * It contains a map of variables to rewrite. This is use to rewrite variables with unwrapped
   * Ref and RefMut types.
   */
  case class EffectsEnv(
    variables: Map[Variable, Set[Target]],
    locals: Map[Identifier, LocalFunDef],
    rewritings: Map[Variable, Variable]
  ) {
    // Adds or updates the targets that a variable points to
    def withVariable(v: Variable, targets: Set[Target]): EffectsEnv =
      copy(variables = variables + (v -> targets))

    // This version is useful for let bindings
    def withVariable(v: Variable, value: Expr)(implicit symbols: Symbols): EffectsEnv = {
      val targets = getTargets(value, this)
      if (targets.isEmpty) withVariable(v, Set(Target(v, None, Path.empty)))
      else withVariable(v, targets)
    }

    // This is useful for adding variables that are roots (i.e. don't refer to anything else than themselves)
    def withRoots(vs: Set[ValDef]): EffectsEnv =
      copy(variables = variables ++ vs.map(v => v.toVariable -> Set(Target(v.toVariable, None, Path.empty))))

    // Adds local functions to the environment
    def withLocals(fds: Seq[LocalFunDef]): EffectsEnv =
      copy(locals = locals ++ fds.map(fd => fd.id -> fd))

    // Adds a variable rewriting to the environment
    def withRewriting(from: Variable, to: Variable): EffectsEnv =
      copy(rewritings = rewritings + (from -> to))

    // Rewrites a variable using the mappings in the environment
    def rewrite(v: Variable): Variable =
      rewritings.get(v).getOrElse(v)

    // This allows to propagate targets for variables that were already
    // declared in the environment. If the target variable is not in the
    // environment, an exception is thrown.
    def propagateTarget(t: Target): Set[Target] =
      variables.get(t.receiver)
        .map(ts => ts.map(_.append(t)))
        .getOrElse(throw MalformedStainlessCode(t.receiver, s"${t.receiver} cannot be mutated"))

    def +(mappings: Map[Variable, Set[Target]]): EffectsEnv = copy(variables = variables ++ mappings)
  }

  object EffectsEnv {
    def empty = EffectsEnv(Map.empty, Map.empty, Map.empty)
  }

  /**
   * This returns all the targets that would be mutated if the given expression was assigned to.
   * For example, given the expression x.y[0], the target would be Target(x, None, .y[0]).
   * If branching occurs, then several targets are returned.
   */
  def getTargets(expr: Expr, env: EffectsEnv)(implicit symbols: Symbols): Set[Target] = {
    def error(expr: Expr, msg: String) =
      throw MalformedStainlessCode(expr, msg)

    def rec(expr: Expr, path: Seq[Accessor])(implicit env: EffectsEnv): Set[Target] = expr match {
      // Variables might refer to the environment, so we need to propagate them
      case v: Variable =>
        env.propagateTarget(Target(v, None, Path(path)))

      // The reference ASTs are only a mean to gather more information, they are aren't
      // semantically useful
      case Ref(e) => rec(e, path)
      case RefMut(e) => rec(e, path)
      case Deref(e) => rec(e, path)
      
      // Those are the ways a path can increase
      case ADTSelector(e, id) => rec(e, ADTFieldAccessor(id) +: path)
      case ClassSelector(e, id) => rec(e, ClassFieldAccessor(id) +: path)
      case ArraySelect(a, idx) => rec(a, ArrayAccessor(idx) +: path)
      case MutableMapApply(m, key) => rec(m, MutableMapAccessor(key) +: path)
      case MutableMapDuplicate(m) => rec(m, path)

      // Those are the ways a path can decrease
      case ADT(id, _, args) => path match {
        case ADTFieldAccessor(fid) +: rest =>
          rec(args(symbols.getConstructor(id).fields.indexWhere(_.id == fid)), rest)
        case _ =>
          Set.empty
      }

      case ClassConstructor(ct, args) => path match {
        case ClassFieldAccessor(fid) +: rest =>
          rec(args(ct.tcd.fields.indexWhere(_.id == fid)), rest)
        case _ =>
          Set.empty
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
        if (symbols.isRecursive(fi.id)) error(expr, "Recursive functions returning mutable references are not allowed")
        else exprOps.withoutSpecs(symbols.simplifyLets(fi.inlined))
          .map(rec(_, path))
          .getOrElse(error(expr, "Functions returning mutable references are not allowed to have no body"))

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

      // In blocks, the last expression is the only one that matters
      case Block(_, last) => rec(last, path)

      // For now this case allows to catch all unimplemented features nicely
      case _ => error(expr, "The target detection was not implemented for that type of expression")
    }

    rec(expr, Seq.empty)(env)
  }

  /**
   * Transforms an assignment of the form expr.deref = value into an equivalent statement.
   */
  def transformDerefAssignment(expr: Expr, value: Expr): Expr = {
    // Returns whether an expression can be assigned to (or in C terms, is an lvalue)
    def isAssignable(expr: Expr): Boolean = expr match {
      // Variables are the roots of assignable expressions
      case v: Variable => true

      // The following cases preserve assignability
      case ADTSelector(e, id) => isAssignable(e)
      case ClassSelector(e, id) => isAssignable(e)
      case ArraySelect(a, idx) => isAssignable(a)
      case MutableMapApply(m, key) => isAssignable(m)

      // The rest makes the expression unassignable (for now)
      case _ => false
    }

    expr match {
      // Variables assignments can be tranformed to simple assignments
      case v: Variable => Assignment(v, value)

      // Those cases can be transformed into assignments if they are assignable
      case ADTSelector(e, id) if isAssignable(e) => FieldAssignment(e, id, value)
      case ClassSelector(e, id) if isAssignable(e) => FieldAssignment(e, id, value)
      case ArraySelect(a, idx) if isAssignable(a) => ArrayUpdate(a, idx, value)
      case MutableMapApply(m, key) if isAssignable(m) => MutableMapUpdate(m, key, value)

      // Other cases cannot be assigned
      case _ => throw FatalError(s"$expr is not assignable")
    }
  }

  protected def typeToAccessor(tpe: Type, id: Identifier)(implicit s: Symbols): Accessor = tpe match {
    case at: ADTType   => ADTFieldAccessor(id)
    case ct: ClassType => ClassFieldAccessor(id)
    case ta: TypeApply => typeToAccessor(ta.getType, id)
    case _ => throw FatalError(s"Cannot have accessors over type $tpe")
  }

  /**
   * This computes which parameters are passed by value (i.e. not by reference).
   */
  def byValParams(params: Seq[ValDef])(implicit symbols: Symbols): Seq[ValDef] = params.filter(_.tpe match {
    case RefType(_) => false
    case RefMutType(_) => false
    case _ => true
  })

  /**
   * This computes which parameters are passed by reference.
   */
  def byRefParams(params: Seq[ValDef])(implicit symbols: Symbols): Seq[ValDef] = params.filter(_.tpe match {
    case RefType(_) => true
    case _ => false
  })

  /**
   * This computes which parameters are passed by mutable reference.
   */
  def byRefMutParams(params: Seq[ValDef])(implicit symbols: Symbols): Seq[ValDef] = params.filter(_.tpe match {
    case RefMutType(_) => true
    case _ => false
  })

  /**
   * This type is used to split the parameters of a function according to their categories
   * (by val, by reference, by mutable reference)
   */
  type SplitParams = (Seq[ValDef], Seq[ValDef], Seq[ValDef])

  /**
   * This function splits the params into their categories
   */
  def split(params: Seq[ValDef])(implicit symbols: Symbols): SplitParams =
    (byValParams(params), byRefParams(params), byRefMutParams(params))

  /**
   * Returns whether the function can return a value that refers to some of its environment,
   * and that can be mutated. In our simple model, it is only the case if the function
   * returns a mutable reference.
   */
  def createsAliasing(ft: FunctionType)(implicit symbols: Symbols): Boolean = ft.to match {
    case RefMutType(_) => true
    case _ => false
  }
}
