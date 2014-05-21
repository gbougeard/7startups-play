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
    community("Workers Union",        Cost(Set(), Funding(1)), Some(perCard(VictoryPoint(1), neighbors, Set(BaseResource)))),  // "FFDOM"
    community("Hipster Bar",          Cost(Set(), Funding(1)), Some(perCard(VictoryPoint(2), neighbors, Set(AdvancedResource)))),  // "FFOO"
    community("Caviar Restaurant",    Cost(Set(), Funding(1)), Some(perCard(VictoryPoint(1), neighbors, Set(Commercial)))),  // "YVA"
    community("Train club",           Cost(Set(), Funding(1)), Some(perCard(VictoryPoint(1), neighbors, Set(ResearchDevelopment)))),  // "DDDAV"
    community("No-Poaching Agreement",Cost(Set(), Funding(1)), Some(perCard(VictoryPoint(1), neighbors, Set(HeadHunting)))),  // "DDDY"
    community("Gloating Party",       Cost(Set(), Funding(1)), Some(AddVictory(CommunityVictory, VictoryPoint(1), ByPoachingResult(neighbors, Set(Defeat))))),  // "FFOA"
    community("Secret Society",       Cost(Set(), Funding(1)), Some(perCard(VictoryPoint(1), myself,    Set(BaseResource,AdvancedResource,Community)))),  // "MMMVY"
    community("Science Club",         Cost(Set(), Funding(1)), Some(ScientificBreakthrough)),  // "MMFFV"
    community("Legal Department",     Cost(Set(), Funding(1)), Some(perCard(VictoryPoint(1), neighbors, Set(Commercial)))),  // "DDDAV"
    community("Company Monument",     Cost(Set(), Funding(1)), Some(AddVictory(CommunityVictory, VictoryPoint(1), ByStartupStage(everyone))))  // "OODDY"
  )

  def community(name: String, cost: Cost, effect: Option[Effect]): Card =
    Card(cName = name,
      cMinPlayers = PlayerCount(0),
      cAge = Age3,
      cType = Community,
      cCost = cost,
      cFree = None,
      cEffect = effect)

  def perCard(victoryPoint: VictoryPoint, target: Target, cardType: Set[CardType]): Effect =
    AddVictory(CommunityVictory, victoryPoint, PerCard(target, cardType))
}
