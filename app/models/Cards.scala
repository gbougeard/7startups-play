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


  case class Card(cName: String,
                  cMinPlayers: PlayerCount,
                  cAge: Age,
                  cType: CardType,
                  cCost: Cost,
                  cFree: Option[String],
                  cEffect: Option[Effect]
                   )

  sealed case class CompanyCard(cCompany: CompanyProfile,
                                cStage: CompanyStage,
                                cCost: Cost,
                                cEffect: Effect)
  
 
   lazy val myself:Target = Set(Own)
   lazy val neighbors:Target = Set(NLeft, NRight)
   lazy val everyone = myself ++ neighbors


}

