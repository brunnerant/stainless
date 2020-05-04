/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package extraction
package imperative

import inox.FatalError

trait AntiAliasing
  extends EffectsAnalyzer
    with oo.CachingPhase
    with SimplyCachedFunctions
    with oo.IdentityTypeDefs
    with SimpleSorts
    with SimplyCachedSorts
    with oo.SimpleClasses
    with oo.SimplyCachedClasses { self =>
  val t: s.type
  import s._

  override protected type TransformerContext = Symbols
  override protected def getContext(symbols: Symbols) = symbols

  override protected type FunctionResult = FunDef
  override protected def registerFunctions(symbols: t.Symbols, functions: Seq[FunDef]): t.Symbols =
    symbols.withFunctions(functions)

  /** This transformer can be used to remove the explicit references */
  object RefRemover extends SelfTreeTransformer {
    override def transform(expr: Expr): Expr = expr match {
      case Ref(e) => transform(e)
      case RefMut(e) => transform(e)
      case Deref(e) => transform(e)

      case _ => super.transform(expr)
    }

    override def transform(tpe: Type): Type = tpe match {
      case RefType(t) => transform(t)
      case RefMutType(t) => transform(t)
      case _ => super.transform(tpe)
    }
  }

  override protected def extractFunction(symbols: Symbols, fd: FunDef): FunctionResult = {
    import symbols._

    /** Returns the new return type of a function having effects */
    def getReturnType(mutatedParams: Seq[ValDef], returnType: Type): Type =
      RefRemover.transform(tupleTypeWrap(returnType +: mutatedParams.map(_.tpe)))

    /** Returns a function type whose return type reflects effects */
    def getFunctionType(fd: FunAbstraction): FunctionType =
      FunctionType(fd.params.map(_.tpe), getReturnType(byRefMutParams(fd.params), fd.returnType))

    /**
     * Given the mutated parameters of a function and its body, wraps the body by creating
     * variables (var) for each mutated param, and returning them in a tuple along with
     * the value returned by the original body.
     */
    def wrapBody(body: Expr, params: SplitParams, env: EffectsEnv): Expr = {
      val (byVal, byRef, byRefMut) = params

      val freshByVal = byVal.map(v => RefRemover.transform(v.freshen))
      val freshByRef = byRef.map(v => RefRemover.transform(v))
      val freshByRefMut = byRefMut.map(v => RefRemover.transform(v.freshen))
      
      def subst(from: Seq[ValDef], to: Seq[ValDef]): Map[Variable, Variable] =
        from.zip(to).map(p => p._1.toVariable -> p._2.toVariable).toMap

      val freshSubst = subst(byVal, freshByVal) ++ subst(byRef, freshByRef) ++ subst(byRefMut, freshByRefMut)

      val freshBody = exprOps.replaceFromSymbols(freshSubst, body)
      val newEnv = env.withRoots((freshByVal ++ freshByRefMut).toSet)
      val explicitBody = makeSideEffectsExplicit(freshBody, newEnv)

      //WARNING: only works if side effects in Tuples are extracted from left to right,
      //         in the ImperativeTransformation phase.
      val finalBody: Expr =
        if (byRefMut.isEmpty) explicitBody
        else Tuple(explicitBody +: freshByRefMut.map(_.toVariable)).copiedFrom(body)

      (freshByVal ++ freshByRefMut).zip(byVal ++ byRefMut).foldRight(finalBody) {
        (vp, bd) => LetVar(vp._1, RefRemover.transform(vp._2.toVariable), bd).copiedFrom(body)
      }
    }

    /**
     * Create a new FunDef for a given FunDef in the program.
     * Adapt the signature to express its effects. In case the
     * function has no effect, this will still return the original
     * FunDef.
     *
     * Also update FunctionType parameters that need to become explicit
     * about the effect they could perform (returning any mutable type that
     * they receive).
     */
    def updateFunction(fd: FunAbstraction, env: EffectsEnv): FunAbstraction = {
      val splitParams @ (byVal, byRef, byRefMut) = split(fd.params)

      // The function now returns the mutated params, so the return types needs to be reflected
      val newReturnType = getReturnType(byRefMut, fd.returnType)

      val newFd = fd.copy(
        returnType = newReturnType,
        params = fd.params.map(RefRemover.transform),
        tparams = fd.tparams.map(RefRemover.transform)
      )

      val (specs, body) = exprOps.deconstructSpecs(fd.fullBody)
      val newBody = body.map(wrapBody(_, splitParams, env))

      val newSpecs = specs.map {
        case exprOps.Postcondition(post @ Lambda(Seq(res), postBody)) =>
          val newRes = ValDef(res.id.freshen, newFd.returnType).copiedFrom(res)

          def freshVar(vd: ValDef): Expr =
            RefRemover.transform(vd).toVariable

          val paramSubst = (byVal ++ byRef).map(vd => (vd.toVariable: Expr) -> freshVar(vd)).toMap
          val resultSubst = Map(res.toVariable -> TupleSelect(newRes.toVariable, 1).copiedFrom(res))

          val oldSubst = byRefMut.map(vd => (Old(vd.toVariable): Expr) -> freshVar(vd)).toMap
          val newSubst = byRefMut.zipWithIndex.map { case (vd, i) =>
            (vd.toVariable -> TupleSelect(newRes.toVariable, i + 2).copiedFrom(vd)): (Expr, Expr)
          }

          val newBody = exprOps.replaceSingle(
            paramSubst ++ resultSubst ++ oldSubst ++ newSubst,
            makeSideEffectsExplicit(postBody, env)
          )

          exprOps.Postcondition(Lambda(Seq(newRes), newBody).copiedFrom(post))

        case spec => spec
      }

      newFd.copy(fullBody = exprOps.reconstructSpecs(newSpecs, newBody, newFd.returnType))
    }

    //We turn all local val of mutable objects into vars and explicit side effects
    //using assignments. We also make sure that no aliasing is being done.
    def makeSideEffectsExplicit(body: Expr, env: EffectsEnv): Expr = {

      object transformer extends inox.transformers.Transformer {
        override val s: self.s.type = self.s
        override val t: self.t.type = self.t
        override type Env = EffectsEnv

        def mapApplication(params: Seq[ValDef], args: Seq[Expr], fi: Expr, env: Env): Expr = {
          val byRefMut = byRefMutParams(params)

          // We only need to do something if the function has effects on its arguments
          if (byRefMut.isEmpty) fi
          else {
            // The return type needs to be updated
            val returnType = getReturnType(byRefMut, fi.getType)

            // We create an intermediate variable to store the result
            val res = ValDef.fresh("res", returnType).copiedFrom(fi)

            // For each mutated arg, we extract its new value from the result
            val extractMutations = byRefMut.zipWithIndex.flatMap { case (vd, index) =>
              val arg = args(params.indexOf(vd))
              val targets = getTargets(arg, env)
              targets.map { t =>
                // The new value is the corresponding element of the returned tuple
                val newValue = applyEffect(Effect(t, TupleSelect(res.toVariable, index + 2)))
                Assignment(t.receiver, newValue)
              }
            }

            Let(res, fi, Block(extractMutations, TupleSelect(res.toVariable, 1)))
          }
        }

        override def transform(e: Expr, env: Env): Expr = (e match {
          // Let and LetVar add new mappings to the environment
          case l @ Let(vd, e, b) =>
            val recons = if (isMutableType(vd.tpe)) Let(_, _, _) else LetVar(_, _, _)
            val newVd = RefRemover.transform(vd)
            val newEnv = env.withVariable(vd.toVariable, e).withRewriting(vd.toVariable, newVd.toVariable)
            recons(newVd, transform(e, env), transform(b, newEnv))

          case l @ LetVar(vd, e, b) =>
            val newVd = RefRemover.transform(vd)
            val newEnv = env.withVariable(vd.toVariable, e).withRewriting(vd.toVariable, newVd.toVariable)
            LetVar(newVd, transform(e, env), transform(b, newEnv))

          case v: Variable =>
            env.rewrite(v)

          case l @ LetRec(fds, body) =>
            val newEnv = env withLocals fds
            val nfds = fds.map(fd => updateFunction(Inner(fd), newEnv).toLocal)
            LetRec(nfds, transform(body, newEnv)).copiedFrom(l)

          case up @ ArrayUpdate(a, i, v) =>
            val idx = ValDef.fresh("idx", RefRemover.transform(i.getType))
            val rhs = ValDef.fresh("rhs", RefRemover.transform(v.getType))

            val effects = getTargets(a, env).map(t => Effect(t :+ ArrayAccessor(idx.toVariable), rhs.toVariable))
            val assignments = Block(effects.toSeq.map(applyEffect), UnitLiteral())

            Block(
              Seq(transform(a, env)),
              Let(idx, transform(i, env), Let(rhs, transform(v, env), assignments))
            )

          case up @ MutableMapUpdate(map, k, v) =>
            val key = ValDef.fresh("key", RefRemover.transform(k.getType))
            val rhs = ValDef.fresh("rhs", RefRemover.transform(v.getType))

            val effects = getTargets(map, env).map(t => Effect(t :+ MutableMapAccessor(key.toVariable), rhs.toVariable))
            val assignments = Block(effects.toSeq.map(applyEffect), UnitLiteral())

            Block(
              Seq(transform(map, env)),
              Let(key, transform(k, env), Let(rhs, transform(v, env), assignments))
            )

          case as @ FieldAssignment(o, id, v) =>
            val rhs = ValDef.fresh("rhs", RefRemover.transform(v.getType))

            val effects = getTargets(o, env).map(t => Effect(t :+ typeToAccessor(o.getType, id), rhs.toVariable))
            val assignments = Block(effects.toSeq.map(applyEffect), UnitLiteral())

            Block(
              Seq(transform(o, env)),
              Let(rhs, transform(v, env), assignments)
            )

          case l @ Lambda(params, body) =>
            Lambda(params, wrapBody(body, split(params), env)).copiedFrom(l)

          case fi @ FunctionInvocation(id, tps, args) =>
            val nfi = FunctionInvocation(id, tps, args.map(transform(_, env))).copiedFrom(fi)
            mapApplication(fd.params, args, nfi, env)

          case alr @ ApplyLetRec(id, tparams, tpe, tps, args) =>
            val fd = Inner(env.locals(id))
            val nfi = ApplyLetRec(
              id, tparams,
              getFunctionType(fd).copiedFrom(tpe), tps,
              args.map(transform(_, env))
            ).copiedFrom(alr)

            mapApplication(fd.params, args, nfi, env)

          case app @ Application(callee, args) =>
            val nfi = Application(
              transform(callee, env),
              args.map(transform(_, env))
            ).copiedFrom(app)

            val FunctionType(from, _) = callee.getType
            val params = from.map(tpe => ValDef.fresh("x", tpe))
            mapApplication(params, args, nfi, env)

          case Ref(e) => transform(e, env)
          case RefMut(e) => transform(e, env)
          case Deref(e) => transform(e, env)

          case Operator(es, recons) =>
            recons(es.map(transform(_, env)))
        }).copiedFrom(e)

        override def transform(tpe: Type, env: EffectsEnv): Type =
          RefRemover.transform(tpe)
      }

      transformer.transform(body, env)
    }

    // Given a receiver object (mutable class, array or map, usually as a reference id),
    // and a path of field/index access, build a copy of the original object, with
    // properly updated values
    def applyEffect(effect: Effect): Expr = {
      val newValue = effect.value

      def rec(receiver: Expr, path: Seq[Accessor]): Expr = path match {
        case ADTFieldAccessor(id) :: fs =>
          val adt @ ADTType(_, tps) = receiver.getType
          val tcons = adt.getSort.constructors.find(_.fields.exists(_.id == id)).get
          val r = rec(Annotated(ADTSelector(receiver, id).copiedFrom(newValue), Seq(Unchecked)).copiedFrom(newValue), fs)

          ADT(tcons.id, tps, tcons.definition.fields.map { vd =>
            if (vd.id == id) r
            else Annotated(ADTSelector(receiver, vd.id).copiedFrom(receiver), Seq(Unchecked)).copiedFrom(receiver)
          }).copiedFrom(newValue)

        case ClassFieldAccessor(id) :: fs =>
          val optCd = receiver.getType match {
            case ct: ClassType => classForField(ct, id)
            case tp => throw FatalError(s"Cannot apply ClassFieldAccessor to type $tp")
          }

          val (cd, ct) = optCd.map(cd => (cd, cd.toType)).getOrElse {
            throw FatalError(s"Could find class for type ${receiver.getType}")
          }

          val casted = AsInstanceOf(receiver, cd.toType).copiedFrom(receiver)
          val r = rec(Annotated(ClassSelector(casted, id).copiedFrom(newValue), Seq(Unchecked)).copiedFrom(newValue), fs)

          ClassConstructor(ct, ct.tcd.fields.map { vd =>
            if (vd.id == id) r
            else Annotated(ClassSelector(casted, vd.id).copiedFrom(receiver), Seq(Unchecked)).copiedFrom(receiver)
          }).copiedFrom(newValue)

        case ArrayAccessor(index) :: fs =>
          val r = rec(Annotated(ArraySelect(receiver, index).copiedFrom(newValue), Seq(Unchecked)).copiedFrom(newValue), fs)
          ArrayUpdated(receiver, index, r).copiedFrom(newValue)

        case MutableMapAccessor(index) :: fs =>
          val r = rec(Annotated(MutableMapApply(receiver, index).copiedFrom(newValue), Seq(Unchecked)).copiedFrom(newValue), fs)
          MutableMapUpdated(receiver, index, r).copiedFrom(newValue)

        case Nil => newValue
      }

      val updated = effect.target match {
        case Target(receiver, None, path) =>
          rec(receiver, path.toSeq)

        case Target(receiver, Some(condition), path) =>
          Annotated(
            AsInstanceOf(
              IfExpr(
                condition.setPos(newValue),
                rec(receiver, path.toSeq),
                receiver
              ).copiedFrom(newValue),
              receiver.getType
            ).copiedFrom(newValue),
          Seq(Unchecked)
        ).copiedFrom(newValue)
      }

      Assignment(effect.target.receiver, updated)
    }

    updateFunction(Outer(fd), EffectsEnv.empty).toFun
  }

  override protected def extractSort(symbols: Symbols, sort: ADTSort): SortResult =
    RefRemover.transform(sort)

  override protected def extractClass(symbols: Symbols, cd: ClassDef): ClassResult =
    RefRemover.transform(cd)
}

object AntiAliasing {
  def apply(trees: Trees)(implicit ctx: inox.Context): ExtractionPipeline {
    val s: trees.type
    val t: trees.type
  } = new AntiAliasing {
    override val s: trees.type = trees
    override val t: trees.type = trees
    override val context = ctx
  }
}
