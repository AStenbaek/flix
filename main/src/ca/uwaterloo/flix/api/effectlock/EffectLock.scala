package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{RigidityEnv, Scheme, Type, TypeConstructor}
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.phase.unification.{BoolUnification, EffUnification3, EqualityEnv, Unification}
import ca.uwaterloo.flix.util.Options

import scala.collection.immutable.SortedSet

object EffectLock {

  /**
    * Returns true if `sc1` is unifiable with `sc2` or if `sc1` is a monomorphic downgrade of `sc2`.
    */
  def isSafe(sc1: Scheme, sc2: Scheme): Boolean = {
    unifiableSchemes(sc1, sc2) || isSubset(sc1.base, sc2.base)
  }

  /**
    * Generalize-rule
    *
    * 𝜎1 ⊑ 𝜏2
    * -------
    * 𝜎1 ⪯ 𝜏2
    *
    */
  private def unifiableSchemes(sc1: Scheme, sc2: Scheme): Boolean = {
    implicit val flix: Flix = new Flix().setOptions(Options.Default)
    val renv = RigidityEnv.apply(SortedSet.from(sc2.quantifiers))
    val unification = Unification.fullyUnifyTypes(sc1.base, sc2.base, renv, EqualityEnv.empty)(Scope.Top, flix)
    unification.isDefined
  }

  /**
    * Subset-rule
    *
    * 𝜑 ∪ 𝜑′ ≡ 𝜑′
    * ----------
    * 𝜏1 −→ 𝜏2 \ 𝜑 ⪯ 𝜏1 -→ 𝜏2 \ 𝜑′
    *
    */
  private def isSubset(tpe1: Type, tpe2: Type): Boolean = {
    // 1. Types match t1 -> t2
    val isMatchingArgs = tpe1.arrowArgTypes == tpe2.arrowArgTypes
    val isMatchingResult = tpe1.arrowResultType == tpe2.arrowResultType

    // TODO: What about type variables? Alpha equivalence

    // 2. Boolean unification of effects phi + phi' = phi'
    val sc1Effs = tpe1.arrowEffectType
    val sc2Effs = tpe2.arrowEffectType
    val left = Type.mkUnion(sc1Effs, sc2Effs, sc1Effs.loc)
    implicit val flix: Flix = new Flix().setOptions(Options.Default)
    val (solution, subst) = EffUnification3.unifyAll(List((left, sc2Effs, sc2Effs.loc)), Scope.Top, RigidityEnv.empty)
    println("new")
    println(s"sc1.base.arrowArgTypes: ${tpe1.arrowArgTypes}")
    println(s"sc1.base.arrowResultType: ${tpe1.arrowResultType}")
    println(s"sc1.base.arrowEffectType: ${tpe1.arrowEffectType}")
    println("old")
    println(s"sc2.base.arrowArgTypes: ${tpe2.arrowArgTypes}")
    println(s"sc2.base.arrowResultType: ${tpe2.arrowResultType}")
    println(s"sc2.base.arrowEffectType: ${tpe2.arrowEffectType}")
    println("---------------")
    println(solution)
    isMatchingArgs && isMatchingResult && solution.isEmpty
  }


  private def monomorphicDowngrade(sc1: Scheme, sc2: Scheme): Boolean = {
    val noQuantifiers = sc1.quantifiers.isEmpty && sc2.quantifiers.isEmpty
    val sameMonomorphicType = checkSameMonomorphicType(sc1.base, sc2.base)
    val subsetOfEffects1 = sc1.base.arrowEffectType.effects.subsetOf(sc2.base.arrowEffectType.effects)
    val subsetOfEffects2 = sc1.base.arrowResultType.effects.subsetOf(sc2.base.arrowResultType.effects)
    noQuantifiers && sameMonomorphicType && subsetOfEffects1 && subsetOfEffects2
  }

  private def checkSameMonomorphicType(tpe1: Type, tpe2: Type): Boolean = (tpe1, tpe2) match {
    // Ignore effect cases
    case (Type.Cst(TypeConstructor.Effect(_), _), _) => true
    case (Type.Cst(TypeConstructor.Pure, _), _) => true
    case (_, Type.Cst(TypeConstructor.Effect(_), _)) => true
    case (_, Type.Cst(TypeConstructor.Pure, _)) => true

    case (Type.Cst(tc1, _), Type.Cst(tc2, _)) =>
      tc1 == tc2

    case (Type.Apply(tpe11, tpe12, _), Type.Apply(tpe21, tpe22, _)) =>
      checkSameMonomorphicType(tpe11, tpe21) && checkSameMonomorphicType(tpe12, tpe22)

    case (Type.Alias(symUse1, args1, tpe1, _), Type.Alias(symUse2, args2, tpe2, _)) =>
      args1.zip(args2).forall {
        case (t1, t2) => checkSameMonomorphicType(t1, t2)
      } && checkSameMonomorphicType(tpe1, tpe2)

    case (Type.AssocType(symUse1, arg1, kind1, _), Type.AssocType(symUse2, arg2, kind2, _)) => ???
    case _ => false
  }
}
