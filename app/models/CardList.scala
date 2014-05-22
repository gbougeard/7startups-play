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
      case CompanyProfile(Microsoft, _) => Stage4
      case _ => Stage3
    }
  }

  def getResourceCard(profile: CompanyProfile, stage: CompanyStage): Card = {
    stage match {
      case Project => {
        val resource: Resource = profile match {
          case CompanyProfile(Facebook, ASide)  => Finance
          case CompanyProfile(Twitter, ASide)   => Youthfulness
          case CompanyProfile(Apple, ASide)     => Vision
          case CompanyProfile(Google, ASide)    => Development
          case CompanyProfile(Yahoo, ASide)     => Marketing
          case CompanyProfile(Amazon, ASide)    => Adoption
          case CompanyProfile(Microsoft, ASide) => Operations
        }
        CompanyCard(profile, Project, Cost.empty, ProvideResource(resource, 1, Shared))

      }
      case Stage1 => {
        val c: Cost = profile match {
          case CompanyProfile(Facebook, ASide)  => cost("MM")
          case CompanyProfile(Twitter, ASide)   => cost("OO")
          case CompanyProfile(Apple, ASide)     => cost("OO")
          case CompanyProfile(Google, ASide)    => cost("DD")
          case CompanyProfile(Yahoo, ASide)     => cost("MM")
          case CompanyProfile(Amazon, ASide)    => cost("DD")
          case CompanyProfile(Microsoft, ASide) => cost("OO")
        }
        CompanyCard(profile, Stage1, c, AddVictory(CompanyVictory, VictoryPoint(3), HappensOnce))
      }
      case Stage2 => {
        val (c, e): (Cost, Effect) = profile match {
          case CompanyProfile(Facebook, ASide)  => (cost("MM"), Poaching(Poacher(2)))
          case CompanyProfile(Twitter, ASide)   => (cost("OO"), ResourceChoice(baseResources, Kept))
          case CompanyProfile(Apple, ASide)     => (cost("OO"), GainFunding(Funding(9), HappensOnce))
          case CompanyProfile(Google, ASide)    => (cost("DD"), ScientificBreakthrough)
          case CompanyProfile(Yahoo, ASide)     => (cost("MM"), Opportunity(Set(Age1, Age2, Age3)))
          case CompanyProfile(Amazon, ASide)    => (cost("DD"), Recycling)
          case CompanyProfile(Microsoft, ASide) => (cost("OO"), AddVictory(CompanyVictory, VictoryPoint(5), HappensOnce))
        }
        CompanyCard(profile, Stage1, c, e)
      }
      case Stage3 => {
        val c: Cost = profile match {
          case CompanyProfile(Facebook, ASide)  => cost("FFFF")
          case CompanyProfile(Twitter, ASide)   => cost("YY")
          case CompanyProfile(Apple, ASide)     => cost("VV")
          case CompanyProfile(Google, ASide)    => cost("DDDD")
          case CompanyProfile(Yahoo, ASide)     => cost("FF")
          case CompanyProfile(Amazon, ASide)    => cost("AA")
          case CompanyProfile(Microsoft, ASide) => cost("OOOO")
        }
        CompanyCard(profile, Stage3, c, AddVictory(CompanyVictory, VictoryPoint(3), HappensOnce))
      }
    }
  }

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
    RegularCard(cName = name,
      cMinPlayers = PlayerCount(0),
      cAge = Age3,
      cType = Community,
      cCost = cost,
      cFree = Set(),
      cEffect = effect)

  def perCard(victoryPoint: VictoryPoint, target: Target, cardType: Set[CardType]): Effect =
    AddVictory(CommunityVictory, victoryPoint, PerCard(target, cardType))
}
