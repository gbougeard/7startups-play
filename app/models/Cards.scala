package models

import models.Base._

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
  sealed trait EffectDirection
  sealed trait Neighboring extends EffectDirection with Neighbor
  object NLeft extends Neighboring
  object NRight extends Neighboring
  object Own extends EffectDirection

  type Target = Set[EffectDirection]

  sealed trait Condition
  object HappensOnce extends Condition
  case class PerCard(target:Target, cardTypes:Set[CardType]) extends Condition
  case class ByPoachingResult(target:Target, poachingOutcomes:Set[PoachingOutcome]) extends Condition
  case class ByStartupStage(target:Target) extends Condition

  sealed trait Sharing
  object Kept extends Sharing

  sealed trait ResearchType
  object Scaling extends ResearchType
  object Programming extends ResearchType
  object CustomSolution extends ResearchType

  sealed trait Effect
  case class ProvideResource(resource:Resource, nb:Int, sharing:Sharing) extends Effect
  case class ResourceChoice(resources:Set[Resource],sharing:Sharing) extends Effect
  case class CheapExchange(resources:Set[Resource], sharing:Sharing, neighbors:Set[Neighbor]) extends Effect
  case class AddVictory(victoryType:VictoryType, victoryPoint:VictoryPoint, condition:Condition) extends Effect
  case class GainFunding(funding:Funding, condition:Condition) extends Effect
  case class RnD(researchType:ResearchType) extends Effect
  case class Poaching(poacher:Poacher) extends Effect
  object ScientificBreakthrough extends Effect
  object Recycling extends Effect
  case class Opportunity(ages:Set[Age]) extends Effect
  object Efficiency extends Effect
  object CopyCommunity extends Effect

  case class Cost(resources:Set[Resource], funding:Funding)

  trait Monoid[Cost] {
    val zero = Cost(Set(), Funding(0))

    def append(s1: Cost, s2: => Cost): Cost = {
      Cost(s1.resources ++ s2.resources, Funding(s1.funding + s2.funding))
    }
  }


  case class Card(cName: String,
                  cMinPlayers: PlayerCount,
                  cAge: Age,
                  cType: CardType,
                  cCost: Cost,
                  cFree: Set[String],
                  cEffect: Set[Effect]
                   )

  sealed case class CompanyCard(cCompany: CompanyProfile,
                                cStage: CompanyStage,
                                cCost: Cost,
                                cEffect: Effect)


  lazy val myself: Target = Set(Own)
  lazy val neighbors: Target = Set(NLeft, NRight)
  lazy val everyone = myself ++ neighbors

  def cost(needs: String): Cost = {
    val costs = needs.map(costBy).toList
    println(costs)
//    costs.foldLeft(Cost(Set(), Funding(0)))(Cost(_ ++ _.resources, Funding(_ + _.funding.value) ))
    costs.head

  }

  def costBy(resource: Char): Cost = {
    resource match {
      case 'Y' => Cost(Set(Youthfullness), Funding(0))
      case 'V' => Cost(Set(Vision), Funding(0))
      case 'A' => Cost(Set(Adoption), Funding(0))
      case 'D' => Cost(Set(Development), Funding(0))
      case 'O' => Cost(Set(Operations), Funding(0))
      case 'M' => Cost(Set(Marketing), Funding(0))
      case 'F' => Cost(Set(Finance), Funding(0))
      case '$' => Cost(Set(), Funding(1))
      case _ => throw new IllegalArgumentException("Invalid cost string")
    }
  }


}

