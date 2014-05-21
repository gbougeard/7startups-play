package models

import models.Base._

/**
 * Created by gbougeard on 21/05/14.
 */
object Cards {

  sealed trait CardType
  sealed trait BaseResource extends CardType
  sealed trait AdvancedResource extends CardType
  sealed trait Infrastructure extends CardType
  sealed trait ResearchDevelopment extends CardType
  sealed trait Commercial extends CardType
  sealed trait HeadHunting extends CardType
  sealed trait Community extends CardType

  sealed trait Neighbor
  sealed trait NLeft extends Neighbor
  sealed trait NRight extends Neighbor

  sealed trait EffectDirection
  sealed trait Neighboring extends EffectDirection with Neighbor
  sealed trait Own extends EffectDirection

  type Target = Set[EffectDirection]

  sealed trait Condition
  object HappensOnce extends Condition
  case class PerCard(target:Target, cardTypes:Set[CardType]) extends Condition
  case class ByPoaching(target:Target, poachingOutcomes:Set[PoachingOutcome]) extends Condition
  case class ByStartupStage(target:Target) extends Condition

  sealed trait Sharing
  sealed trait Kept extends Sharing

  sealed trait ResearchType
  sealed trait Scaling extends ResearchType
  sealed trait Programming extends ResearchType
  sealed trait CustomSolution extends ResearchType

  sealed trait Effect
  case class ProvideResource(resource:Resource, nb:Int, sharing:Sharing) extends Effect
  case class ResourceChoice(resources:Set[Resource],sharing:Sharing) extends Effect
  case class CheapExchange(resources:Set[Resource], sharing:Sharing, neighbors:Set[Neighbor]) extends Effect
  case class AddVictory(victoryType:VictoryType, victoryPoint:VictoryPoint, condition:Condition) extends Effect
  case class GainFunding(funding:Funding, condition:Condition) extends Effect
  case class RnD(researchType:ResearchType) extends Effect
  case class Poaching(poacher:Poacher) extends Effect
  sealed trait ScientificBreakthrough extends Effect
  sealed trait Recycling extends Effect
  case class Opportunity(ages:Set[Age]) extends Effect
  sealed trait Efficiency extends Effect
  sealed trait CopyCommunity extends Effect

  case class Cost(resources:Set[Resource], funding:Funding)



  sealed case class Card(cName: String,
                         cMinPlayers: PlayerCount,
                         cAge: Age,
                         cType: CardType,
                         cCost: Cost,
                         cFree: Option[String],
                         cEffect: Option[Effect]
                          )

  sealed case class CompanyCard(cCompany: CompanyProfile,
                                 cStage:CompanyStage,
                                 cCost : Cost,
                                 cEffect : Effect)

}

