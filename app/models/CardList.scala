package models

import models.Base._
import models.Cards._

/**
 * Created by gbougeard on 21/05/14.
 */
object CardList {

  def getMaxStage(profile: CompanyProfile): CompanyStage = {
    profile match {
      case CompanyProfile(Facebook, BSide) => Stage2
      case CompanyProfile(Microsoft, BSide) => Stage4
      case _ => Stage3
    }
  }

  //  def getResourceCard(profile:CompanyProfile):CompanyStage

  lazy val communities: Set[Card] = Set(
    community("Workers Union",        cost("FFDOM"),  Set(perCard(VictoryPoint(1), neighbors, Set(BaseResource)))),  // "FFDOM"
    community("Hipster Bar",          cost("FFOO"),   Set(perCard(VictoryPoint(2), neighbors, Set(AdvancedResource)))),  // "FFOO"
    community("Caviar Restaurant",    cost("YVA"),    Set(perCard(VictoryPoint(1), neighbors, Set(Commercial)))),  // "YVA"
    community("Train club",           cost("DDDAV"),  Set(perCard(VictoryPoint(1), neighbors, Set(ResearchDevelopment)))),  // "DDDAV"
    community("No-Poaching Agreement",cost("DDDY"),   Set(perCard(VictoryPoint(1), neighbors, Set(HeadHunting)))),  // "DDDY"
    community("Secret Society",       cost("MMMVY"),  Set(perCard(VictoryPoint(1), myself,    Set(BaseResource,AdvancedResource,Community)))),  // "MMMVY"
    community("Legal Department",     cost("DDDAV"),  Set(perCard(VictoryPoint(1), neighbors, Set(Commercial)))),  // "DDDAV"
    community("Science Club",         cost("MMFFV"),  Set(ScientificBreakthrough)),  // "MMFFV"
    community("Gloating Party",       cost("FFOA"),   Set(AddVictory(CommunityVictory, VictoryPoint(1), ByPoachingResult(neighbors, Set(Defeat))))),  // "FFOA"
    community("Company Monument",     cost("OODDY"),  Set(AddVictory(CommunityVictory, VictoryPoint(1), ByStartupStage(everyone))))  // "OODDY"
  )

  def community(name: String, cost: Cost, effect: Set[Effect]): Card =
    Card(cName = name,
      cMinPlayers = PlayerCount(0),
      cAge = Age3,
      cType = Community,
      cCost = cost,
      cFree = Set(),
      cEffect = effect)

  def perCard(victoryPoint: VictoryPoint, target: Target, cardType: Set[CardType]): Effect =
    AddVictory(CommunityVictory, victoryPoint, PerCard(target, cardType))
}
