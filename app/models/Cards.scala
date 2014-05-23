package models

import models.Base._
import scalaz._
import Scalaz._
import scala.collection.Bag

/**
 * Created by gbougeard on 21/05/14.
 */
object Cards {

  sealed trait CardType
  case object BaseResource extends CardType         // The "brown" cards, provide basic resources
  case object AdvancedResource extends CardType     // The "grey" cards, provide advanced resources
  case object Infrastructure extends CardType       // The "blue" cards, directly give victory points
  case object ResearchDevelopment extends CardType  // The "green" cards, the more you have, the more victory points you get
  case object Commercial extends CardType           // The "gold" cards, mainly get you funding
  case object HeadHunting extends CardType          // The "red" cards, giving poaching power
  case object Community extends CardType            // The "purple" cards, giving victory points according to various conditions

  sealed trait Neighbor
  case object NLeft extends Neighbor
  case object NRight extends Neighbor
  sealed trait EffectDirection
  case class Neighboring(neighbor:Neighbor) extends EffectDirection
  case object Own extends EffectDirection

  type Target = Set[EffectDirection]

  sealed trait Condition
  case object HappensOnce extends Condition
  case class PerCard(target:Target, cardTypes:Set[CardType]) extends Condition
  case class ByPoachingResult(target:Target, poachingOutcomes:Set[PoachingOutcome]) extends Condition
  case class ByStartupStage(target:Target) extends Condition

  sealed trait Sharing
  case object Shared extends Sharing
  case object Kept extends Sharing

  sealed trait ResearchType
  case object Scaling extends ResearchType
  case object Programming extends ResearchType
  case object CustomSolution extends ResearchType

  sealed trait Effect
  case class ProvideResource(resource: Resource, nb: Int, sharing: Sharing) extends Effect
  case class ResourceChoice(resources: Set[Resource], sharing: Sharing) extends Effect
  case class CheapExchange(resources: Set[Resource], neighbors: Set[Neighbor]) extends Effect
  case class AddVictory(victoryType: VictoryType, victoryPoint: VictoryPoint, condition: Condition) extends Effect
  case class GainFunding(funding: Funding, condition: Condition) extends Effect
  case class RnD(researchType: ResearchType) extends Effect
  case class Poaching(poacher: Poacher) extends Effect
  case object ScientificBreakthrough extends Effect
  case object Recycling extends Effect
  case class Opportunity(ages: Set[Age]) extends Effect
  case object Efficiency extends Effect
  case object CopyCommunity extends Effect

//  object Effect {
//     def show(effect: Effect) :String = {
//         effect match {
//           case e: ProvideResource    =>  s"ProvideResource - ${e.resource} : ${e.nb}, ${e.sharing}"
//           case e: ResourceChoice     =>  s"ResourceChoice - ${e.resources}, ${e.sharing}"
//           case e: CheapExchange      =>  s"CheapExchange - ${e.resources}, ${e.neighbors}"
//           case e: AddVictory         =>  s"AddVictory - ${e.victoryType} : ${e.victoryPoint} if ${e.condition}"
//           case e: GainFunding        =>  s"GainFunding - ${e.funding}  if ${e.condition}"
//           case e: RnD                =>  s"RnD - ${e.researchType}"
//           case e: Poaching           =>  s"Poaching - ${e.poacher}"
//           case  ScientificBreakthrough =>s"ScientificBreakthrough"
//           case  Recycling            =>  s"Recycling"
//           case e: Opportunity        =>  s"Opportunity at ${e.ages}"
//           case  Efficiency           =>  s"Efficiency"
//           case  CopyCommunity        =>  s"CopyCommunity"
//         }
//     }
//  }

  implicit val bagResource = Bag.configuration.compact[Resource]
  case class Cost(resources:Bag[Resource], funding:Funding)
  object Cost {
    def empty = Cost(Bag.empty, 0)
  }

  implicit object CostMonoid extends Monoid[Cost]{
    override def zero: Cost = Cost.empty
    override def append(f1: Cost, f2: => Cost): Cost =  Cost(f1.resources ++ f2.resources, f1.funding + f2.funding)
  }

  sealed trait Card

  case class RegularCard(cName: String,
                         cMinPlayers: PlayerCount,
                         cAge: Age,
                         cType: CardType,
                         cCost: Cost,
                         cFree: Set[String],
                         cEffects: Set[Effect]) extends Card

  case class CompanyCard(cCompany: CompanyProfile,
                         cStage: CompanyStage,
                         cCost: Cost,
                         cEffects: Set[Effect]) extends Card

  type Exchange = Map[Neighbor, Bag[Resource]]


  lazy val myself: Target = Set(Own)
  lazy val neighbors: Target = Set(Neighboring(NLeft), Neighboring(NRight))
  lazy val everyone = myself ++ neighbors

  def cost(needs: String): Cost = {
    needs.toList.foldMap(toCost)
  }

  def toCost(resource: Char): Cost = {
    resource match {
      case 'Y' => Cost(Bag(Youthfulness), 0)
      case 'V' => Cost(Bag(Vision), 0)
      case 'A' => Cost(Bag(Adoption), 0)
      case 'D' => Cost(Bag(Development), 0)
      case 'O' => Cost(Bag(Operations), 0)
      case 'M' => Cost(Bag(Marketing), 0)
      case 'F' => Cost(Bag(Finance), 0)
      case '$' => Cost(Bag.empty, 1)
      case _ => throw new IllegalArgumentException("Invalid cost string")
    }
  }


}

