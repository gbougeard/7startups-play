package models

import models.Base._
import scalaz.{Foldable, Monoid}
import scalaz._
import Scalaz._
import scala.collection.Bag

/**
 * Created by gbougeard on 21/05/14.
 */
object Cards {

  sealed trait CardType
  object BaseResource extends CardType
  object AdvancedResource extends CardType
  object Infrastructure extends CardType
  object ResearchDevelopment extends CardType
  object Commercial extends CardType
  object HeadHunting extends CardType
  object Community extends CardType

  sealed trait Neighbor
  object NLeft extends Neighbor
  object NRight extends Neighbor
  sealed trait EffectDirection
  case class Neighboring(neighbor:Neighbor) extends EffectDirection
  object Own extends EffectDirection

  type Target = Set[EffectDirection]

  sealed trait Condition
  object HappensOnce extends Condition
  case class PerCard(target:Target, cardTypes:Set[CardType]) extends Condition
  case class ByPoachingResult(target:Target, poachingOutcomes:Set[PoachingOutcome]) extends Condition
  case class ByStartupStage(target:Target) extends Condition

  sealed trait Sharing
  object Shared extends Sharing
  object Kept extends Sharing

  sealed trait ResearchType
  object Scaling extends ResearchType
  object Programming extends ResearchType
  object CustomSolution extends ResearchType

  sealed trait Effect
  case class ProvideResource(resource:Resource, nb:Int, sharing:Sharing) extends Effect
  case class ResourceChoice(resources:Set[Resource],sharing:Sharing) extends Effect
  case class CheapExchange(resources:Set[Resource], neighbors:Set[Neighbor]) extends Effect
  case class AddVictory(victoryType:VictoryType, victoryPoint:VictoryPoint, condition:Condition) extends Effect
  case class GainFunding(funding:Funding, condition:Condition) extends Effect
  case class RnD(researchType:ResearchType) extends Effect
  case class Poaching(poacher:Poacher) extends Effect
  object ScientificBreakthrough extends Effect
  object Recycling extends Effect
  case class Opportunity(ages:Set[Age]) extends Effect
  object Efficiency extends Effect
  object CopyCommunity extends Effect

  implicit val bagResource = Bag.configuration.compact[Resource]
  case class Cost(resources:Bag[Resource], funding:Funding)

  object Cost {
    def empty = Cost(Bag.empty, Funding(0))
  }

  implicit object CostMonoid extends Monoid[Cost]{
    override def zero: Cost = Cost.empty

    override def append(f1: Cost, f2: => Cost): Cost =  Cost(f1.resources ++ f2.resources, Funding(f1.funding.value + f2.funding.value))

  }

//  implicit def CostSemigroup: Semigroup[Cost] = semigroup((f1, f2) => Cost(f1.resources ++ f2.resources, Funding(f1.funding.value + f2.funding.value)))
//  implicit def CostZero: Zero[Cost] = zero(Cost(Set(), Funding(0)))

//  trait Monoid[Cost] {
//    val zero = Cost(Set(), Funding(0))
//
//    def append(s1: Cost, s2: => Cost): Cost = {
//      Cost(s1.resources ++ s2.resources, Funding(s1.funding + s2.funding))
//    }
//  }

  sealed trait Card
  case class RegularCard(cName: String,
                  cMinPlayers: PlayerCount,
                  cAge: Age,
                  cType: CardType,
                  cCost: Cost,
                  cFree: Set[String],
                  cEffects: Set[Effect]
                   ) extends Card

  case class CompanyCard(cCompany: CompanyProfile,
                                cStage: CompanyStage,
                                cCost: Cost,
                                cEffects: Set[Effect]) extends Card


  lazy val myself: Target = Set(Own)
  lazy val neighbors: Target = Set(Neighboring(NLeft), Neighboring(NRight))
  lazy val everyone = myself ++ neighbors

  def cost(needs: String): Cost = {
    needs.toList.foldMap(toCost)
  }

  def toCost(resource: Char): Cost = {
    resource match {
      case 'Y' => Cost(Bag(Youthfulness), Funding(0))
      case 'V' => Cost(Bag(Vision), Funding(0))
      case 'A' => Cost(Bag(Adoption), Funding(0))
      case 'D' => Cost(Bag(Development), Funding(0))
      case 'O' => Cost(Bag(Operations), Funding(0))
      case 'M' => Cost(Bag(Marketing), Funding(0))
      case 'F' => Cost(Bag(Finance), Funding(0))
      case '$' => Cost(Bag.empty, Funding(1))
      case _ => throw new IllegalArgumentException("Invalid cost string")
    }
  }


}

