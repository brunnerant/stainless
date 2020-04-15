/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package extraction
package imperative

import inox.FatalError

trait EffectsAnalyzer {
  val s: Trees
  import s._
  import exprOps._

  sealed abstract class Accessor
  case class ADTFieldAccessor(selector: Identifier) extends Accessor
  case class ClassFieldAccessor(selector: Identifier) extends Accessor
  case class ArrayAccessor(index: Expr) extends Accessor
  case class MutableMapAccessor(index: Expr) extends Accessor

  case class Path(path: Seq[Accessor]) {
    def :+(elem: Accessor): Path = Path(path :+ elem)
    def +:(elem: Accessor): Path = Path(elem +: path)
    def ++(that: Path): Path = Path(this.path ++ that.path)

    def on(that: Expr)(implicit symbols: Symbols): Set[Target] = {
      def rec(expr: Expr, path: Seq[Accessor]): Option[Expr] = path match {
        case ADTFieldAccessor(id) +: xs =>
          rec(ADTSelector(expr, id), xs)

        case ClassFieldAccessor(id) +: xs =>
          def asClassType(tpe: Type): Option[ClassType] = tpe match {
            case ct: ClassType => Some(ct)
            case ta: TypeApply if !ta.isAbstract => asClassType(ta.resolve)
            case other => None
          }

          for {
            ct  <- asClassType(expr.getType)
            tcd <- symbols.classForField(ct, id)
            res <- rec(ClassSelector(AsInstanceOf(expr, tcd.toType), id), xs)
          } yield res

        case ArrayAccessor(idx) +: xs =>
          rec(ArraySelect(expr, idx), xs)

        case MutableMapAccessor(idx) +: xs =>
          rec(MutableMapApply(expr, idx), xs)

        case Seq() =>
          Some(expr)
      }

      rec(that, path).toSet.flatMap(getTargets)
    }

    def prefixOf(that: Path): Boolean = {
      def rec(p1: Seq[Accessor], p2: Seq[Accessor]): Boolean = (p1, p2) match {
        case (Seq(), _) => true
        case (ArrayAccessor(_) +: xs1, ArrayAccessor(_) +: xs2) =>
          rec(xs1, xs2)
        case (ADTFieldAccessor(id1) +: xs1, ADTFieldAccessor(id2) +: xs2) if id1 == id2 =>
          rec(xs1, xs2)
        case (ClassFieldAccessor(id1) +: xs1, ClassFieldAccessor(id2) +: xs2) if id1 == id2 =>
          rec(xs1, xs2)
        case (MutableMapAccessor(_) +: xs1, MutableMapAccessor(_) +: xs2) =>
          rec(xs1, xs2)
        case _ => false
      }

      rec(path, that.path)
    }

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
    def +(elem: Accessor): Target = Target(receiver, condition, path :+ elem)

    def append(that: Target): Target = (condition, that.condition) match {
      case (condition, None) =>
        Target(receiver, condition, path ++ that.path)
      case (None, _) =>
        Target(receiver, that.condition, path ++ that.path)
      case (Some(condition), Some(thatCondition)) =>
        Target(receiver, Some(And(condition, thatCondition)), path ++ that.path)
    }

    def prependPath(path: Path): Target = Target(receiver, condition, path ++ this.path)

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
   * An environment is a mapping between variables and targets. This allows to keep track of the
   * roots of all assignments. For example, if a variable is declared as such:
   * val y = x.<path>
   * then the environment will contain a pair y -> Target(x, None, <path>)
   */
  case class Env(variables: Map[Variable, Set[Target]]) {
    // This allows to propagate targets for variables that were already
    // declared in the environment. Targets that are not in the environment
    // are left untouched.
    def propagateTarget(t: Target): Set[Target] =
      variables.get(t.receiver)
        .map(ts => ts.map(t.prependPath(_)))
        .getOrElse(t)

    def +(mappings: Map[Variable, Set[Target]]): Env = copy(variables = variables ++ mappings)
  }

  /**
   * This returns all the targets that would be mutated if the given expression was assigned to.
   * For example, given the expression x.y[0], the target would be Target(x, .y[0]).
   * If branching occurs, then several targets are returned.
   */
  def getTargets(expr: Expr, env: Env)(implicit symbols: Symbols): Set[Target] = {
    def error(expr: Expr) =
      throw MalformedStainlessCode(expr, s"Couldn't compute targets in: $expr")

    def rec(expr: Expr, path: Seq[Accessor])(implicit env: Env): Set[Target] = expr match {
      // Variables might refer to the environment
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

      // If-then-else's introduce conditionnal effects
      case IfExpr(cnd, thn, els) =>
        def conj(cnd1: Expr, cnd2: Option[Expr]): Expr = cnd2 map { cnd2 =>
          And(cnd1, cnd2.setPos(cnd1)).setPos(cnd1)
        } getOrElse(cnd1)

        rec(thn, path).map(t => Target(t.receiver, Some(conj(cnd, t.condition), t.path)) ++
        rec(els, path).map(e => Target(e.receiver, Some(conj(Not(cnd).setPos(cnd), e.condition)), e.path))

      case m: MatchExpr =>
        rec(symbols.matchToIfThenElse(m), path)

      // Non recursive functions can be inlined to make the translation more permissive
      case fi: FunctionInvocation if !symbols.isRecursive(fi.id) =>
        exprOps.withoutSpecs(symbols.simplifyLets(fi.inlined))
          .map(rec(_, path))
          .getOrElse(error(expr))

      // For recursive function, we take a conservative approach and only look at the argument types.
      // If there is an effect on any of the arguments, we abort.
      case fi: FunctionInvocation
        if (functionTypeEffects(fi.type) isEmpty) Set.empty
        else error(expr)

      // Lets add variables to the environment
      case Let(vd, e, b) =>
        val exprTargets = rec(e, Seq.empty)
        rec(b, path)(env + Map(vd.toVariable -> exprTargets))

      // For now this case allows to catch all unimplemented features nicely
      case _ => error(expr)
    }

    rec(expr, Seq.empty)(env)
  }

  def getExactEffects(expr: Expr)(implicit symbols: Symbols): Set[Target] = getEffects(expr) match {
    case effects if effects.nonEmpty => effects
    case _ => throw MalformedStainlessCode(expr, s"Couldn't compute exact effect targets in: $expr")
  }

  def getKnownEffects(expr: Expr)(implicit symbols: Symbols): Set[Target] = try {
    getEffects(expr)
  } catch {
    case _: MalformedStainlessCode => Set.empty
  }

  protected def typeToAccessor(tpe: Type, id: Identifier)(implicit s: Symbols): Accessor = tpe match {
    case at: ADTType   => ADTFieldAccessor(id)
    case ct: ClassType => ClassFieldAccessor(id)
    case ta: TypeApply => typeToAccessor(ta.getType, id)
    case _ => throw FatalError(s"Cannot have accessors over type $tpe")
  }

  /** Return all effects of expr
    *
    * Effects of expr are any free variables in scope (either local vars
    * already defined in the scope containing expr, or global var) that
    * are re-assigned by an operation in the expression. An effect is
    * also a mutation of an object refer by an id defined in the scope.
    *
    * This is a conservative analysis, not taking into account control-flow.
    * The set of effects is not definitely effects, but any identifier
    * not in the set will for sure have no effect.
    *
    * We are assuming no aliasing.
    */
  private def expressionEffects(expr: Expr, result: Result)(implicit symbols: Symbols): Set[Effect] = {
    import symbols._
    val freeVars = variablesOf(expr)

    def inEnv(effect: Effect, env: Map[Variable, Effect]): Option[Effect] =
      env.get(effect.receiver).map(e => e.copy(path = e.path ++ effect.path))

    def targets(expr: Expr, env: Map[Variable, Effect]): Set[Effect] =
      getTargets(expr) flatMap { (target: Target) =>
        inEnv(target.toEffect, env).toSet
      }

    def rec(expr: Expr, env: Map[Variable, Effect]): Set[Effect] = expr match {
      case Let(vd, e, b) if symbols.isMutableType(vd.tpe) =>
        // BUG: here the .map(...) loses some information, which disallows certain valid
        // programs to be transformed. The fix would require env to be of type
        // Map[Variable, Set[Effect]] to capture all the effects on the free variables.
        rec(e, env) ++ rec(b, env ++ targets(e, env).map(vd.toVariable -> _))

      case MatchExpr(scrut, cses) if symbols.isMutableType(scrut.getType) =>
        rec(scrut, env) ++ cses.flatMap { case MatchCase(pattern, guard, rhs) =>
          val newEnv = env ++ mapForPattern(scrut, pattern).flatMap {
            case (v, e) => targets(e, env).map(v.toVariable -> _)
          }
          guard.toSeq.flatMap(rec(_, newEnv)).toSet ++ rec(rhs, newEnv)
        }

      case ArrayUpdate(o, idx, v) =>
        rec(o, env) ++ rec(idx, env) ++ rec(v, env) ++
        targets(o, env).map(_ + ArrayAccessor(idx))

      case MutableMapUpdate(map, key, value) =>
        rec(map, env) ++ rec(key, env) ++ rec(value, env) ++
        targets(map, env).map(_ + MutableMapAccessor(key))

      case MutableMapUpdated(map, key, value) =>
        rec(map, env) ++ rec(key, env) ++ rec(value, env)

      case MutableMapDuplicate(map) =>
        rec(map, env)

      case fa @ FieldAssignment(o, id, v) =>
        val accessor = typeToAccessor(o.getType, id)
        rec(o, env) ++ rec(v, env) ++ targets(o, env).map(_ + accessor)

      case Application(callee, args) =>
        val ft @ FunctionType(_, _) = callee.getType
        val effects = functionTypeEffects(ft)

        // The second part are the potential side-effects while evaluating the arguments
        // The last part are the arguments that might be mutated inside the function
        rec(callee, env) ++ args.flatMap(rec(_, env)) ++
        args.map(targets(_, env)).zipWithIndex
          .filter(p => effects contains p._2)
          .flatMap(_._1)

      case Assignment(v, value) => rec(value, env) ++ env.get(v)

      case IfExpr(cnd, thn, els) =>
        rec(cnd, env) ++ rec(thn, env) ++ rec(els, env)

      case fi @ FunInvocation(id, tps, args, _) =>
        val fun = fi match {
          case FunctionInvocation(id, _, _) => Outer(getFunction(id))
          case ApplyLetRec(id, _, _, _, _) => result.locals(id)
        }

        val currentEffects: Set[Effect] = result.effects(fun)
        val paramSubst = (fun.params.map(_.toVariable) zip args).toMap
        val invocEffects = currentEffects.flatMap(e => paramSubst.get(e.receiver) match {
          case Some(arg) => (e on arg).flatMap(inEnv(_, env))
          case None => Seq(e) // This effect occurs on some variable captured from scope
        })

        val effectsOnFreeVars = invocEffects.filter(e => freeVars contains e.receiver)
        val effectsOnLocalFreeVars = currentEffects.filterNot(e => paramSubst contains e.receiver)
        effectsOnFreeVars ++ effectsOnLocalFreeVars ++ args.flatMap(rec(_, env))

      case Operator(es, _) => es.flatMap(rec(_, env)).toSet
    }

    val mutated = rec(expr, freeVars.map(v => v -> Effect(v, Path.empty)).toMap)

    // We truncate the effects path if it goes through an inductive ADT as
    // such effects can lead to inexistence of the effects fixpoint.
    // We also truncate array paths as they rely on some index that is not
    // necessarily well-scoped (and could itself have effects).
    def truncate(effect: Effect): Effect = {
      def isInductive(tpe: Type, seen: Set[Identifier]): Boolean = {
        val deps = s.typeOps.collect {
          case ADTType(id, _) => dependencies(id)
          case ClassType(id, _) => dependencies(id)
          case _ => Set.empty[Identifier]
        } (tpe)

        (seen & deps).nonEmpty
      }

      def rec(tpe: Type, path: Seq[Accessor], seen: Set[Identifier]): Seq[Accessor] = (tpe, path) match {
        case (adt: ADTType, (fa @ ADTFieldAccessor(id)) +: xs) =>
          val field = adt.getSort.constructors.flatMap(_.fields).find(_.id == id).get
          if (isInductive(field.getType, seen)) Seq()
          else fa +: rec(field.getType, xs, seen + adt.id)
        case (ct: ClassType, (fa @ ClassFieldAccessor(id)) +: xs) =>
          val field = getClassField(ct, id).get
          if (isInductive(field.getType, seen)) Seq()
          else fa +: rec(field.getType, xs, seen + ct.id)
        case (_, ArrayAccessor(_) +: xs) => Seq()
        case _ => Seq()
      }

      Effect(effect.receiver, Path(rec(effect.receiver.getType, effect.path.toSeq, Set())))
    }

    // We merge paths that are prefixes of one another or point to the same array
    def merge(paths: Set[Path]): Set[Path] = {
      // This truncates the path `p2` depending on `p1`
      def rec(p1: Seq[Accessor], p2: Seq[Accessor]): Option[Path] = (p1, p2) match {
        case (ArrayAccessor(idx1) +: xs1, ArrayAccessor(idx2) +: xs2) if idx1 != idx2 => Some(Path.empty)
        case (x1 +: xs1, x2 +: xs2) if x1 == x2 => rec(xs1, xs2).map(x1 +: _)
        case (Nil, Nil) => Some(Path.empty)
        case _ => None
      }

      val merged = paths.flatMap { t1 =>
        paths.flatMap { t2 =>
          rec(t1.toSeq, t2.toSeq)
        } + t1
      }

      merged.filterNot(t1 => (merged - t1).exists(t2 => t2 prefixOf t1))
    }

    mutated
      .map(truncate)
      .groupBy(_.receiver)
      .flatMap { case (v, effects) => merge(effects.map(_.path)).map(Effect(v, _)) }.toSet
  }

  /** 
   * This computes the effects by looking at the function type. It takes a conservative
   * approach and detects effects as soon as an argument has a mutable reference type.
   */
  def functionTypeEffects(ft: FunctionType): Set[Int] = {
    ft.from.zipWithIndex.flatMap {
      case (tpe: RefMutType, i) => Some(i)
      case _ => None
    }.toSet
  }
}
