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

  def getResourceCard(profile: CompanyProfile, stage: CompanyStage): Card = {
    val aSideEffects : Set[Effect] = Set(AddVictory(CompanyVictory, VictoryPoint(3), HappensOnce))

    stage match {
      case Project => {
        val resource: Resource = profile match {
          case CompanyProfile(Facebook, _)  => Finance
          case CompanyProfile(Twitter, _)   => Youthfulness
          case CompanyProfile(Apple, _)     => Vision
          case CompanyProfile(Google, _)    => Development
          case CompanyProfile(Yahoo, _)     => Marketing
          case CompanyProfile(Amazon, _)    => Adoption
          case CompanyProfile(Microsoft, _) => Operations
        }
        CompanyCard(profile, Project, Cost.empty, Set(ProvideResource(resource, 1, Shared)))

      }
      case Stage1 => {
        val (c, e): (Cost, Set[Effect]) = profile match {
          case CompanyProfile(Facebook, ASide)  => (cost("MM"),   aSideEffects)
          case CompanyProfile(Facebook, BSide)  => (cost("OOO "), Set(Poaching(Poacher(1)),
                                                                      AddVictory(CompanyVictory, VictoryPoint(3), HappensOnce),
                                                                      GainFunding(Funding(3), HappensOnce)))

          case CompanyProfile(Twitter, ASide)   => (cost("OO"),   aSideEffects)
          case CompanyProfile(Twitter, BSide)   => (cost("DD"),   Set(ResourceChoice(baseResources, Kept)))

          case CompanyProfile(Apple, ASide)     => (cost("OO"),   aSideEffects)
          case CompanyProfile(Apple, BSide)     => (cost("OO"),   Set(AddVictory(CompanyVictory, VictoryPoint(2), HappensOnce),
                                                                      GainFunding(Funding(4), HappensOnce)))

          case CompanyProfile(Google, ASide)    => (cost("DD"),   aSideEffects)
          case CompanyProfile(Google, BSide)    => (cost("DA"),   Set(AddVictory(CompanyVictory, VictoryPoint(3), HappensOnce)))

          case CompanyProfile(Yahoo, ASide)     => (cost("MM"),   aSideEffects)
          case CompanyProfile(Yahoo, BSide)     => (cost("MM"),   Set(CheapExchange(baseResources, Set(NLeft, NRight))))

          case CompanyProfile(Amazon, ASide)    => (cost("DD"),   aSideEffects)
          case CompanyProfile(Amazon, BSide)    => (cost("FF"),   Set(AddVictory(CompanyVictory, VictoryPoint(2), HappensOnce),
                                                                       Recycling))

          case CompanyProfile(Microsoft, ASide) => (cost("OO"),   aSideEffects)
          case CompanyProfile(Microsoft, BSide) => (cost("MM"), Set(AddVictory(CompanyVictory, VictoryPoint(3), HappensOnce)))
          }
          CompanyCard(profile, Stage1, c, e)
      }
      case Stage2 => {
        val (c, e): (Cost, Set[Effect]) = profile match {
          case CompanyProfile(Facebook, ASide)  => (cost("MM"), Set(Poaching(Poacher(2))))
          case CompanyProfile(Facebook, BSide)  => (cost("FFFF"), Set(Poaching(Poacher(1)),
                                                                      AddVictory(CompanyVictory, VictoryPoint(4), HappensOnce),
                                                                      GainFunding(Funding(4), HappensOnce)))

          case CompanyProfile(Twitter, ASide)   => (cost("OO"), Set(ResourceChoice(baseResources, Kept)))
          case CompanyProfile(Twitter, BSide)   => (cost("MM"), Set(ResourceChoice(advancedResources, Kept)))

          case CompanyProfile(Apple, ASide)     => (cost("OO"), Set(GainFunding(Funding(9), HappensOnce)))
          case CompanyProfile(Apple, BSide)     => (cost("MM"), Set(AddVictory(CompanyVictory, VictoryPoint(3), HappensOnce),
                                                                    GainFunding(Funding(4), HappensOnce)))

          case CompanyProfile(Google, ASide)    => (cost("DD"), Set(ScientificBreakthrough))
          case CompanyProfile(Google, BSide)    => (cost("MMY"), Set(Efficiency))

          case CompanyProfile(Yahoo, ASide)     => (cost("MM"), Set(Opportunity(Set(Age1, Age2, Age3))))
          case CompanyProfile(Yahoo, BSide)     => (cost("OO"), Set(AddVictory(CompanyVictory, VictoryPoint(5), HappensOnce)))

          case CompanyProfile(Amazon, ASide)    => (cost("DD"),   Set(Recycling))
          case CompanyProfile(Amazon, BSide)    => (cost("DDD"),  Set(AddVictory(CompanyVictory, VictoryPoint(1), HappensOnce),
                                                                      Recycling))

          case CompanyProfile(Microsoft, ASide) => (cost("OO"), Set(AddVictory(CompanyVictory, VictoryPoint(5), HappensOnce)))
          case CompanyProfile(Microsoft, BSide) => (cost("OOO"), Set(AddVictory(CompanyVictory, VictoryPoint(5), HappensOnce)))
        }
        CompanyCard(profile, Stage1, c, e)
      }
      case Stage3 => {
        val (c, e): (Cost, Set[Effect]) = profile match {
          case CompanyProfile(Facebook, ASide)  => (cost("FFFF"), aSideEffects)

          case CompanyProfile(Twitter, ASide)   => (cost("YY"), aSideEffects)
          case CompanyProfile(Twitter, BSide)   => (cost("OOO"), Set( AddVictory(CompanyVictory, VictoryPoint(7), HappensOnce)))

          case CompanyProfile(Apple, ASide)     => (cost("VV"), aSideEffects)
          case CompanyProfile(Apple, BSide)     => (cost("YVA"), Set(AddVictory(CompanyVictory, VictoryPoint(5), HappensOnce),
                                                                     GainFunding(Funding(4), HappensOnce)))

          case CompanyProfile(Google, ASide)    => (cost("DDDD"), aSideEffects)
          case CompanyProfile(Google, BSide)    => (cost("DDDV"), Set(ScientificBreakthrough))

          case CompanyProfile(Yahoo, ASide)     => (cost("FF"), aSideEffects)
          case CompanyProfile(Yahoo, BSide)     => (cost("FFA"), Set(CopyCommunity))

          case CompanyProfile(Amazon, ASide)    => (cost("AA"), aSideEffects)
          case CompanyProfile(Amazon, BSide)    => (cost("YVA"), Set(Recycling))

          case CompanyProfile(Microsoft, ASide) => (cost("OOOO"), aSideEffects)
          case CompanyProfile(Microsoft, BSide) => (cost("OOO"), Set(AddVictory(CompanyVictory, VictoryPoint(5), HappensOnce)))
        }
        CompanyCard(profile, Stage3, c, e)
      }
      case Stage4 => {
        val (c, e): (Cost, Set[Effect]) = profile match {
          case CompanyProfile(Microsoft, BSide) => (cost("OOOOA"), Set(AddVictory(CompanyVictory, VictoryPoint(7), HappensOnce)))
        }
        CompanyCard(profile, Stage4, c, e)
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

  def community(name: String, cost: Cost, effects: Set[Effect]): Card =
    RegularCard(cName = name,
      cMinPlayers = PlayerCount(0),
      cAge = Age3,
      cType = Community,
      cCost = cost,
      cFree = Set(),
      cEffects = effects)

  def perCard(victoryPoint: VictoryPoint, target: Target, cardType: Set[CardType]): Effect =
    AddVictory(CommunityVictory, victoryPoint, PerCard(target, cardType))
}
