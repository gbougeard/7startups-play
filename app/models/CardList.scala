package models

import models.Base._
import models.Cards._
import models.Cards.ByPoachingResult
import models.Cards.Poaching
import models.Cards.CheapExchange
import models.Cards.CompanyCard
import models.Base.Poacher
import models.Cards.RnD
import models.Cards.PerCard
import models.Base.Funding
import models.Cards.ResourceChoice
import models.Cards.GainFunding
import models.Base.VictoryPoint
import models.Cards.RegularCard
import models.Cards.Opportunity
import models.Cards.AddVictory
import models.Cards.ProvideResource
import models.Base.PlayerCount
import models.Cards.ByStartupStage
import models.Base.CompanyProfile

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
    val aSideEffects : Set[Effect] = Set(AddVictory(CompanyVictory, 3, HappensOnce))

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
          case _ => throw new IllegalArgumentException("Invalid card")
        }
        CompanyCard(profile, Project, Cost.empty, Set(ProvideResource(resource, 1, Shared)))

      }
      case Stage1 => {
        val (c, e): (Cost, Set[Effect]) = profile match {
          case CompanyProfile(Facebook, ASide)  => (cost("MM"),   aSideEffects)
          case CompanyProfile(Facebook, BSide)  => (cost("OOO "), Set(Poaching(1),
                                                                      AddVictory(CompanyVictory, 3, HappensOnce),
                                                                      GainFunding(3, HappensOnce)))

          case CompanyProfile(Twitter, ASide)   => (cost("OO"),   aSideEffects)
          case CompanyProfile(Twitter, BSide)   => (cost("DD"),   Set(ResourceChoice(baseResources, Kept)))

          case CompanyProfile(Apple, ASide)     => (cost("OO"),   aSideEffects)
          case CompanyProfile(Apple, BSide)     => (cost("OO"),   Set(AddVictory(CompanyVictory, 2, HappensOnce),
                                                                      GainFunding(4, HappensOnce)))

          case CompanyProfile(Google, ASide)    => (cost("DD"),   aSideEffects)
          case CompanyProfile(Google, BSide)    => (cost("DA"),   Set(AddVictory(CompanyVictory, 3, HappensOnce)))

          case CompanyProfile(Yahoo, ASide)     => (cost("MM"),   aSideEffects)
          case CompanyProfile(Yahoo, BSide)     => (cost("MM"),   Set(CheapExchange(baseResources, Set(NLeft, NRight))))

          case CompanyProfile(Amazon, ASide)    => (cost("DD"),   aSideEffects)
          case CompanyProfile(Amazon, BSide)    => (cost("FF"),   Set(AddVictory(CompanyVictory, 2, HappensOnce),
                                                                       Recycling))

          case CompanyProfile(Microsoft, ASide) => (cost("OO"),   aSideEffects)
          case CompanyProfile(Microsoft, BSide) => (cost("MM"), Set(AddVictory(CompanyVictory, 3, HappensOnce)))

          case _ => throw new IllegalArgumentException("Invalid card")
          }
          CompanyCard(profile, Stage1, c, e)
      }
      case Stage2 => {
        val (c, e): (Cost, Set[Effect]) = profile match {
          case CompanyProfile(Facebook, ASide)  => (cost("MM"), Set(Poaching(2)))
          case CompanyProfile(Facebook, BSide)  => (cost("FFFF"), Set(Poaching(1),
                                                                      AddVictory(CompanyVictory, 4, HappensOnce),
                                                                      GainFunding(4, HappensOnce)))

          case CompanyProfile(Twitter, ASide)   => (cost("OO"), Set(ResourceChoice(baseResources, Kept)))
          case CompanyProfile(Twitter, BSide)   => (cost("MM"), Set(ResourceChoice(advancedResources, Kept)))

          case CompanyProfile(Apple, ASide)     => (cost("OO"), Set(GainFunding(9, HappensOnce)))
          case CompanyProfile(Apple, BSide)     => (cost("MM"), Set(AddVictory(CompanyVictory, 3, HappensOnce),
                                                                    GainFunding(4, HappensOnce)))

          case CompanyProfile(Google, ASide)    => (cost("DD"), Set(ScientificBreakthrough))
          case CompanyProfile(Google, BSide)    => (cost("MMY"), Set(Efficiency))

          case CompanyProfile(Yahoo, ASide)     => (cost("MM"), Set(Opportunity(Set(Age1, Age2, Age3))))
          case CompanyProfile(Yahoo, BSide)     => (cost("OO"), Set(AddVictory(CompanyVictory, 5, HappensOnce)))

          case CompanyProfile(Amazon, ASide)    => (cost("DD"),   Set(Recycling))
          case CompanyProfile(Amazon, BSide)    => (cost("DDD"),  Set(AddVictory(CompanyVictory, 1, HappensOnce),
                                                                      Recycling))

          case CompanyProfile(Microsoft, ASide) => (cost("OO"), Set(AddVictory(CompanyVictory, 5, HappensOnce)))
          case CompanyProfile(Microsoft, BSide) => (cost("OOO"), Set(AddVictory(CompanyVictory, 5, HappensOnce)))

          case _ => throw new IllegalArgumentException("Invalid card")
        }
        CompanyCard(profile, Stage1, c, e)
      }
      case Stage3 => {
        val (c, e): (Cost, Set[Effect]) = profile match {
          case CompanyProfile(Facebook, ASide)  => (cost("FFFF"), aSideEffects)

          case CompanyProfile(Twitter, ASide)   => (cost("YY"), aSideEffects)
          case CompanyProfile(Twitter, BSide)   => (cost("OOO"), Set( AddVictory(CompanyVictory, 7, HappensOnce)))

          case CompanyProfile(Apple, ASide)     => (cost("VV"), aSideEffects)
          case CompanyProfile(Apple, BSide)     => (cost("YVA"), Set(AddVictory(CompanyVictory, 5, HappensOnce),
                                                                     GainFunding(4, HappensOnce)))

          case CompanyProfile(Google, ASide)    => (cost("DDDD"), aSideEffects)
          case CompanyProfile(Google, BSide)    => (cost("DDDV"), Set(ScientificBreakthrough))

          case CompanyProfile(Yahoo, ASide)     => (cost("FF"), aSideEffects)
          case CompanyProfile(Yahoo, BSide)     => (cost("FFA"), Set(CopyCommunity))

          case CompanyProfile(Amazon, ASide)    => (cost("AA"), aSideEffects)
          case CompanyProfile(Amazon, BSide)    => (cost("YVA"), Set(Recycling))

          case CompanyProfile(Microsoft, ASide) => (cost("OOOO"), aSideEffects)
          case CompanyProfile(Microsoft, BSide) => (cost("OOO"), Set(AddVictory(CompanyVictory, 5, HappensOnce)))

          case _ => throw new IllegalArgumentException("Invalid card")
        }
        CompanyCard(profile, Stage3, c, e)
      }
      case Stage4 => {
        val (c, e): (Cost, Set[Effect]) = profile match {
          case CompanyProfile(Microsoft, BSide) => (cost("OOOOA"), Set(AddVictory(CompanyVictory, 7, HappensOnce)))
          case _ => throw new IllegalArgumentException("Invalid card")
        }
        CompanyCard(profile, Stage4, c, e)
      }
      case _ => throw new IllegalArgumentException("Invalid card")
    }
  }

  lazy val communities: Set[Card] = Set(
    community("Workers Union",        cost("FFDOM"),  Set(perCard(1, neighbors, Set(BaseResource)))),  // "FFDOM"
    community("Hipster Bar",          cost("FFOO"),   Set(perCard(2, neighbors, Set(AdvancedResource)))),  // "FFOO"
    community("Caviar Restaurant",    cost("YVA"),    Set(perCard(1, neighbors, Set(Commercial)))),  // "YVA"
    community("Train club",           cost("DDDAV"),  Set(perCard(1, neighbors, Set(ResearchDevelopment)))),  // "DDDAV"
    community("No-Poaching Agreement",cost("DDDY"),   Set(perCard(1, neighbors, Set(HeadHunting)))),  // "DDDY"
    community("Secret Society",       cost("MMMVY"),  Set(perCard(1, myself,    Set(BaseResource,AdvancedResource,Community)))),  // "MMMVY"
    community("Legal Department",     cost("DDDAV"),  Set(perCard(1, neighbors, Set(Commercial)))),  // "DDDAV"
    community("Science Club",         cost("MMFFV"),  Set(ScientificBreakthrough)),  // "MMFFV"
    community("Gloating Party",       cost("FFOA"),   Set(AddVictory(CommunityVictory, 1, ByPoachingResult(neighbors, Set(Defeat))))),  // "FFOA"
    community("Company Monument",     cost("OODDY"),  Set(AddVictory(CommunityVictory, 1, ByStartupStage(everyone))))  // "OODDY"
  )

  def community(name: String, cost: Cost, effects: Set[Effect]): Card =
    RegularCard(cName = name,
      cMinPlayers = 0,
      cAge = Age3,
      cType = Community,
      cCost = cost,
      cFree = Set(),
      cEffects = effects)

  def perCard(victoryPoint: VictoryPoint, target: Target, cardType: Set[CardType]): Effect =
    AddVictory(CommunityVictory, victoryPoint, PerCard(target, cardType))
  
  lazy val allCards :Set[Card] = Set(
    RegularCard( "Marketroid"            ,3, Age1, BaseResource, cost( "" ), Set(),Set(ProvideResource( Marketing ,1, Shared)))
  , RegularCard( "Marketroid"            ,4, Age1, BaseResource, cost( "" ), Set(),Set(ProvideResource( Marketing ,1, Shared)))
  , RegularCard( "IT Technician"         ,3, Age1, BaseResource, cost( "" ), Set(),Set(ProvideResource( Operations ,1, Shared)))
  , RegularCard( "IT Technician"         ,5, Age1, BaseResource, cost( "" ), Set(),Set(ProvideResource( Operations ,1, Shared)))
  , RegularCard( "Enterprise Programmer" ,3, Age1, BaseResource, cost( "" ), Set(),Set(ProvideResource( Development ,1, Shared)))
  , RegularCard( "Enterprise Programmer" ,5, Age1, BaseResource, cost( "" ), Set(),Set(ProvideResource( Development ,1, Shared)))
  , RegularCard( "Accountant"            ,3, Age1, BaseResource, cost( "" ), Set(),Set(ProvideResource( Finance ,1, Shared)))
  , RegularCard( "Accountant"            ,4, Age1, BaseResource, cost( "" ), Set(),Set(ProvideResource( Finance ,1, Shared)))
  , RegularCard( "Feature Driven Team"   ,6, Age1, BaseResource, cost( "$"), Set(),Set(ResourceChoice (Set(Marketing, Development), Shared)))
  , RegularCard( "Operations Guru"       ,3, Age1, BaseResource, cost( "$"), Set(),Set(ResourceChoice (Set(Marketing, Operations), Shared)))
  , RegularCard( "Value Optimizer"       ,5, Age1, BaseResource, cost( "$"), Set(),Set(ResourceChoice (Set(Marketing, Finance), Shared)))
  , RegularCard( "Devops Team"           ,4, Age1, BaseResource, cost( "$"), Set(),Set(ResourceChoice (Set(Operations, Development), Shared)))
  , RegularCard( "Financial Developer"   ,3, Age1, BaseResource, cost( "$"), Set(),Set(ResourceChoice (Set(Development, Finance), Shared)))
  , RegularCard( "High Frequency Trader" ,6, Age1, BaseResource, cost( "$"), Set(),Set(ResourceChoice (Set(Finance, Operations), Shared)))
  , RegularCard( "Marketing Expert"      ,3, Age2, BaseResource, cost( "$"), Set(),Set(ProvideResource( Marketing, 2, Shared)))
  , RegularCard( "Marketing Expert"      ,4, Age2, BaseResource, cost( "$"), Set(),Set(ProvideResource( Marketing, 2, Shared)))
  , RegularCard( "IT Architect"          ,3, Age2, BaseResource, cost( "$"), Set(),Set(ProvideResource( Operations, 2, Shared)))
  , RegularCard( "IT Architect"          ,4, Age2, BaseResource, cost( "$"), Set(),Set(ProvideResource( Operations, 2, Shared)))
  , RegularCard( "Functional Programmer" ,3, Age2, BaseResource, cost( "$"), Set(),Set(ProvideResource( Development, 2, Shared)))
  , RegularCard( "Functional Programmer" ,4, Age2, BaseResource, cost( "$"), Set(),Set(ProvideResource( Development, 2, Shared)))
  , RegularCard( "Double Irish Expert"   ,3, Age2, BaseResource, cost( "$"), Set(),Set(ProvideResource( Finance, 2, Shared)))
  , RegularCard( "Double Irish Expert"   ,4, Age2, BaseResource, cost( "$"), Set(),Set(ProvideResource( Finance, 2, Shared)))

  , RegularCard( "Rock Star Evangelist"  ,3, Age1, AdvancedResource, cost( ""), Set(), Set(ProvideResource( Adoption ,1, Shared)))
  , RegularCard( "Rock Star Evangelist"  ,6, Age1, AdvancedResource, cost( ""), Set(), Set(ProvideResource( Adoption ,1, Shared)))
  , RegularCard( "Rock Star Evangelist"  ,3, Age2, AdvancedResource, cost( ""), Set(), Set(ProvideResource( Adoption ,1, Shared)))
  , RegularCard( "Rock Star Evangelist"  ,5, Age2, AdvancedResource, cost( ""), Set(), Set(ProvideResource( Adoption ,1, Shared)))
  , RegularCard( "Company Nerf Battles"  ,3, Age1, AdvancedResource, cost( ""), Set(), Set(ProvideResource( Youthfulness, 1, Shared)))
  , RegularCard( "Company Nerf Battles"  ,6, Age1, AdvancedResource, cost( ""), Set(), Set(ProvideResource( Youthfulness, 1, Shared)))
  , RegularCard( "Company Nerf Battles"  ,3, Age2, AdvancedResource, cost( ""), Set(), Set(ProvideResource( Youthfulness, 1, Shared)))
  , RegularCard( "Company Nerf Battles"  ,5, Age2, AdvancedResource, cost( ""), Set(), Set(ProvideResource( Youthfulness, 1, Shared)))
  , RegularCard( "Charismatic Leader"    ,3, Age1, AdvancedResource, cost( ""), Set(), Set(ProvideResource( Vision, 1, Shared)))
  , RegularCard( "Charismatic Leader"    ,6, Age1, AdvancedResource, cost( ""), Set(), Set(ProvideResource( Vision, 1, Shared)))
  , RegularCard( "Charismatic Leader"    ,3, Age2, AdvancedResource, cost( ""), Set(), Set(ProvideResource( Vision, 1, Shared)))
  , RegularCard( "Charismatic Leader"    ,5, Age2, AdvancedResource, cost( ""), Set(), Set(ProvideResource( Vision, 1, Shared)))

  , RegularCard( "High Speed Internet"   ,4, Age1, Infrastructure, cost( ""       ), Set(                        ),Set(AddVictory(InfrastructureVictory, 3, HappensOnce)))
  , RegularCard( "High Speed Internet"   ,7, Age1, Infrastructure, cost( ""       ), Set(                        ),Set(AddVictory(InfrastructureVictory, 3, HappensOnce)))
  , RegularCard( "Admin Network"         ,3, Age1, Infrastructure, cost( "O"      ), Set("Operations Center"     ),Set(AddVictory(InfrastructureVictory, 3, HappensOnce)))
  , RegularCard( "Admin Network"         ,7, Age1, Infrastructure, cost( "O"      ), Set("Operations Center"     ),Set(AddVictory(InfrastructureVictory, 3, HappensOnce)))
  , RegularCard( "Operations Center"     ,3, Age2, Infrastructure, cost( "OOO"    ), Set(                        ),Set(AddVictory(InfrastructureVictory, 5, HappensOnce)))
  , RegularCard( "Operations Center"     ,7, Age2, Infrastructure, cost( "OOO"    ), Set(                        ),Set(AddVictory(InfrastructureVictory, 5, HappensOnce)))
  , RegularCard( "Cloud Servers"         ,3, Age1, Infrastructure, cost( ""       ), Set("Collocated Datacenter" ),Set(AddVictory(InfrastructureVictory, 2, HappensOnce)))
  , RegularCard( "Cloud Servers"         ,5, Age1, Infrastructure, cost( ""       ), Set("Collocated Datacenter" ),Set(AddVictory(InfrastructureVictory, 2, HappensOnce)))
  , RegularCard( "Collocated Datacenter" ,3, Age2, Infrastructure, cost( "MDY"    ), Set("Company Datacenter"    ),Set(AddVictory(InfrastructureVictory, 3, HappensOnce)))
  , RegularCard( "Collocated Datacenter" ,6, Age2, Infrastructure, cost( "MDY"    ), Set("Company Datacenter"    ),Set(AddVictory(InfrastructureVictory, 3, HappensOnce)))
  , RegularCard( "Company Datacenter"    ,3, Age3, Infrastructure, cost( "DDFYVA" ), Set(                        ),Set(AddVictory(InfrastructureVictory, 7, HappensOnce)))
  , RegularCard( "Company Datacenter"    ,6, Age3, Infrastructure, cost( "DDFYVA" ), Set(                        ),Set(AddVictory(InfrastructureVictory, 7, HappensOnce)))
  , RegularCard( "Garage"                ,3, Age1, Infrastructure, cost( ""       ), Set("Office"                ),Set(AddVictory(InfrastructureVictory, 2, HappensOnce)))
  , RegularCard( "Garage"                ,6, Age1, Infrastructure, cost( ""       ), Set("Office"                ),Set(AddVictory(InfrastructureVictory, 2, HappensOnce)))
  , RegularCard( "Office"                ,3, Age2, Infrastructure, cost( "FFM"    ), Set("Company Building"      ),Set(AddVictory(InfrastructureVictory, 4, HappensOnce)))
  , RegularCard( "Office"                ,7, Age2, Infrastructure, cost( "FFM"    ), Set("Company Building"      ),Set(AddVictory(InfrastructureVictory, 4, HappensOnce)))
  , RegularCard( "Company Building"      ,3, Age3, Infrastructure, cost( "DDM"    ), Set(                        ),Set(AddVictory(InfrastructureVictory, 5, HappensOnce)))
  , RegularCard( "Company Building"      ,4, Age3, Infrastructure, cost( "DDM"    ), Set(                        ),Set(AddVictory(InfrastructureVictory, 5, HappensOnce)))
  , RegularCard( "Custom Routers"        ,3, Age2, Infrastructure, cost( "DDA"    ), Set(                        ),Set(AddVictory(InfrastructureVictory, 4, HappensOnce)))
  , RegularCard( "Custom Routers"        ,5, Age2, Infrastructure, cost( "DDA"    ), Set(                        ),Set(AddVictory(InfrastructureVictory, 4, HappensOnce)))
  , RegularCard( "Custom Servers"        ,3, Age3, Infrastructure, cost( "MMFO"   ), Set(                        ),Set(AddVictory(InfrastructureVictory, 6, HappensOnce)))
  , RegularCard( "Custom Servers"        ,5, Age3, Infrastructure, cost( "MMFO"   ), Set(                        ),Set(AddVictory(InfrastructureVictory, 6, HappensOnce)))
  , RegularCard( "National Fiber Network",3, Age3, Infrastructure, cost( "OOFY"   ), Set(                        ),Set(AddVictory(InfrastructureVictory, 6, HappensOnce)))
  , RegularCard( "National Fiber Network",5, Age3, Infrastructure, cost( "OOFY"   ), Set(                        ),Set(AddVictory(InfrastructureVictory, 6, HappensOnce)))
  , RegularCard( "National Fiber Network",6, Age3, Infrastructure, cost( "OOFY"   ), Set(                        ),Set(AddVictory(InfrastructureVictory, 6, HappensOnce)))
  , RegularCard( "Lavish Headquarters"   ,3, Age3, Infrastructure, cost( "YVADOMF"), Set(                        ),Set(AddVictory(InfrastructureVictory, 8, HappensOnce)))
  , RegularCard( "Lavish Headquarters"   ,7, Age3, Infrastructure, cost( "YVADOMF"), Set(                        ),Set(AddVictory(InfrastructureVictory, 8, HappensOnce)))

  , RegularCard( "Free Drinks"            , 3, Age1, HeadHunting, cost("M"    ),Set()                    , Set(Poaching(1)))
  , RegularCard( "Free Drinks"            , 7, Age1, HeadHunting, cost("M"    ),Set()                    , Set(Poaching(1)))
  , RegularCard( "Standing Desks"         , 3, Age1, HeadHunting, cost("F"    ),Set()                    , Set(Poaching(1)))
  , RegularCard( "Standing Desks"         , 5, Age1, HeadHunting, cost("F"    ),Set()                    , Set(Poaching(1)))
  , RegularCard( "Free Food"              , 3, Age1, HeadHunting, cost("D"    ),Set()                    , Set(Poaching(1)))
  , RegularCard( "Free Food"              , 4, Age1, HeadHunting, cost("D"    ),Set()                    , Set(Poaching(1)))
  , RegularCard( "Car Fleet"              , 3, Age2, HeadHunting, cost("OOO"  ),Set("Segways")           , Set(Poaching(2)))
  , RegularCard( "Car Fleet"              , 7, Age2, HeadHunting, cost("OOO"  ),Set("Segways")           , Set(Poaching(2)))
  , RegularCard( "Segways"                , 3, Age3, HeadHunting, cost("FFFO" ),Set()                    , Set(Poaching(3)))
  , RegularCard( "Segways"                , 7, Age3, HeadHunting, cost("FFFO" ),Set()                    , Set(Poaching(3)))
  , RegularCard( "Nap Rooms"              , 4, Age2, HeadHunting, cost("FFM"  ),Set("High-Tech Toilets") , Set(Poaching(2)))
  , RegularCard( "Nap Rooms"              , 6, Age2, HeadHunting, cost("FFM"  ),Set("High-Tech Toilets") , Set(Poaching(2)))
  , RegularCard( "Nap Rooms"              , 7, Age2, HeadHunting, cost("FFM"  ),Set("High-Tech Toilets") , Set(Poaching(2)))
  , RegularCard( "High-Tech Toilets"      , 4, Age3, HeadHunting, cost("OOOF" ),Set()                    , Set(Poaching(3)))
  , RegularCard( "High-Tech Toilets"      , 5, Age3, HeadHunting, cost("OOOF" ),Set()                    , Set(Poaching(3)))
  , RegularCard( "High-Tech Toilets"      , 6, Age3, HeadHunting, cost("OOOF" ),Set()                    , Set(Poaching(3)))
  , RegularCard( "Ball Pits"              , 3, Age3, HeadHunting, cost("FMMA" ),Set()                    , Set(Poaching(3)))
  , RegularCard( "Ball Pits"              , 4, Age3, HeadHunting, cost("FMMA" ),Set()                    , Set(Poaching(3)))
  , RegularCard( "Ball Pits"              , 7, Age3, HeadHunting, cost("FMMA" ),Set()                    , Set(Poaching(3)))
  , RegularCard( "Technology Guru"        , 3, Age2, HeadHunting, cost("FDM"  ),Set()                    , Set(Poaching(2)))
  , RegularCard( "Technology Guru"        , 5, Age2, HeadHunting, cost("FDM"  ),Set()                    , Set(Poaching(2)))
  , RegularCard( "Cool Internal Language" , 3, Age2, HeadHunting, cost("MMF"  ),Set()                    , Set(Poaching(2)))
  , RegularCard( "Cool Internal Language" , 6, Age2, HeadHunting, cost("MMF"  ),Set()                    , Set(Poaching(2)))
  , RegularCard( "Can Work In Haskell"    , 3, Age3, HeadHunting, cost("MDDD" ),Set()                    , Set(Poaching(3)))
  , RegularCard( "Can Work In Haskell"    , 5, Age3, HeadHunting, cost("MDDD" ),Set()                    , Set(Poaching(3)))

  , RegularCard( "Database Sharding"        ,3, Age1, ResearchDevelopment, cost( "A"   ), Set("Global Databases", "Technology Guru"      ), Set(RnD(Scaling       )))
  , RegularCard( "Database Sharding"        ,5, Age1, ResearchDevelopment, cost( "A"   ), Set("Global Databases", "Technology Guru"      ), Set(RnD(Scaling       )))
  , RegularCard( "Global Databases"         ,3, Age2, ResearchDevelopment, cost( "FFY" ), Set("Tech Conference", "Architecture Mastery"  ), Set(RnD(Scaling       )))
  , RegularCard( "Global Databases"         ,4, Age2, ResearchDevelopment, cost( "FFY" ), Set("Tech Conference", "Architecture Mastery"  ), Set(RnD(Scaling       )))
  , RegularCard( "Architecture Mastery"     ,3, Age3, ResearchDevelopment, cost( "DDVA"), Set(                                           ), Set(RnD(Scaling       )))
  , RegularCard( "Architecture Mastery"     ,6, Age3, ResearchDevelopment, cost( "DDVA"), Set(                                           ), Set(RnD(Scaling       )))
  , RegularCard( "Anything But Java"        ,3, Age1, ResearchDevelopment, cost( "Y"   ), Set("Proper Typing", "Cool Internal Language"  ), Set(RnD(Programming   )))
  , RegularCard( "Anything But Java"        ,7, Age1, ResearchDevelopment, cost( "Y"   ), Set("Proper Typing", "Cool Internal Language"  ), Set(RnD(Programming   )))
  , RegularCard( "Proper Typing"            ,3, Age2, ResearchDevelopment, cost( "DDV" ), Set("Functional Mastery", "Can Work In Haskell"), Set(RnD(Programming   )))
  , RegularCard( "Proper Typing"            ,5, Age2, ResearchDevelopment, cost( "DDV" ), Set("Functional Mastery", "Can Work In Haskell"), Set(RnD(Programming   )))
  , RegularCard( "Functional Mastery"       ,3, Age3, ResearchDevelopment, cost( "FFYA"), Set(                                           ), Set(RnD(Programming   )))
  , RegularCard( "Functional Mastery"       ,7, Age3, ResearchDevelopment, cost( "FFYA"), Set(                                           ), Set(RnD(Programming   )))
  , RegularCard( "Hardware Knowledge"       ,3, Age1, ResearchDevelopment, cost( "V"   ), Set("Custom Routers", "Efficiency Advances"    ), Set(RnD(CustomSolution)))
  , RegularCard( "Hardware Knowledge"       ,4, Age1, ResearchDevelopment, cost( "V"   ), Set("Custom Routers", "Efficiency Advances"    ), Set(RnD(CustomSolution)))
  , RegularCard( "Efficiency Advances"      ,3, Age2, ResearchDevelopment, cost( "OOA" ), Set("Custom Servers", "Seven Nines"            ), Set(RnD(CustomSolution)))
  , RegularCard( "Efficiency Advances"      ,6, Age2, ResearchDevelopment, cost( "OOA" ), Set("Custom Servers", "Seven Nines"            ), Set(RnD(CustomSolution)))
  , RegularCard( "Seven Nines"              ,3, Age3, ResearchDevelopment, cost( "MMYV"), Set(                                           ), Set(RnD(CustomSolution)))
  , RegularCard( "Seven Nines"              ,4, Age3, ResearchDevelopment, cost( "MMYV"), Set(                                           ), Set(RnD(CustomSolution)))
  , RegularCard( "Efficient Fiber Networks" ,3, Age2, ResearchDevelopment, cost( "MV"  ), Set("Global Clusters","Cloud Language"         ), Set(RnD(CustomSolution)))
  , RegularCard( "Efficient Fiber Networks" ,7, Age2, ResearchDevelopment, cost( "MV"  ), Set("Global Clusters","Cloud Language"         ), Set(RnD(CustomSolution)))
  , RegularCard( "Global Clusters"          ,3, Age3, ResearchDevelopment, cost( "OOOY"), Set(                                           ), Set(RnD(Scaling       )))
  , RegularCard( "Global Clusters"          ,7, Age3, ResearchDevelopment, cost( "OOOY"), Set(                                           ), Set(RnD(Scaling       )))
  , RegularCard( "Cloud Language"           ,3, Age3, ResearchDevelopment, cost( "MAV" ), Set(                                           ), Set(RnD(Programming   )))
  , RegularCard( "Cloud Language"           ,5, Age3, ResearchDevelopment, cost( "MAV" ), Set(                                           ), Set(RnD(Programming   )))

  , RegularCard( "Business Angel"   , 4, Age1, Commercial, cost(""    ), Set(                   ), Set(GainFunding(5, HappensOnce)))
  , RegularCard( "Business Angel"   , 5, Age1, Commercial, cost(""    ), Set(                   ), Set(GainFunding(5,  HappensOnce)))
  , RegularCard( "Business Angel"   , 7, Age1, Commercial, cost(""    ), Set(                   ), Set(GainFunding(5,  HappensOnce)))
  , RegularCard( "Right Partnership", 3, Age1, Commercial, cost("$"   ), Set("Think Tank"       ), Set(CheapExchange(baseResources, Set(NRight))))
  , RegularCard( "Right Partnership", 7, Age1, Commercial, cost("$"   ), Set("Think Tank"       ), Set(CheapExchange(baseResources, Set(NRight))))
  , RegularCard( "Left Partnership" , 3, Age1, Commercial, cost("$"   ), Set("Think Tank"       ), Set(CheapExchange(baseResources, Set(NLeft) )))
  , RegularCard( "Left Partnership" , 7, Age1, Commercial, cost("$"   ), Set("Think Tank"       ), Set(CheapExchange(baseResources, Set(NLeft) )))
  , RegularCard( "Corporate Spy"    , 3, Age1, Commercial, cost("$"   ), Set("Offshore Labor"   ), Set(CheapExchange(advancedResources,Set(NLeft, NRight))))
  , RegularCard( "Corporate Spy"    , 6, Age1, Commercial, cost("$"   ), Set("Offshore Labor"   ), Set(CheapExchange(advancedResources, Set(NLeft, NRight))))
  , RegularCard( "Think Tank"       , 3, Age2, Commercial, cost("DD"  ), Set("Software Product" ), Set(ResourceChoice (advancedResources, Kept)))
  , RegularCard( "Think Tank"       , 6, Age2, Commercial, cost("DD"  ), Set("Software Product" ), Set(ResourceChoice (advancedResources, Kept)))
  , RegularCard( "Think Tank"       , 7, Age2, Commercial, cost("DD"  ), Set("Software Product" ), Set(ResourceChoice (advancedResources, Kept)))
  , RegularCard( "Offshore Labor"   , 3, Age2, Commercial, cost("MM"  ), Set("Brand Recognition"), Set(ResourceChoice (baseResources, Kept)))
  , RegularCard( "Offshore Labor"   , 5, Age2, Commercial, cost("MM"  ), Set("Brand Recognition"), Set(ResourceChoice (baseResources, Kept)))
  , RegularCard( "Offshore Labor"   , 6, Age2, Commercial, cost("MM"  ), Set("Brand Recognition"), Set(ResourceChoice (baseResources, Kept)))
  , RegularCard( "Development Gig"  , 3, Age2, Commercial, cost(""    ), Set(                   ), Set(GainFunding(1, PerCard( everyone, Set(BaseResource)))))
  , RegularCard( "Development Gig"  , 6, Age2, Commercial, cost(""    ), Set(                   ), Set(GainFunding(1, PerCard( everyone, Set(BaseResource)))))
  , RegularCard( "Consulting Gig"   , 4, Age2, Commercial, cost(""    ), Set(                   ), Set(GainFunding(2, PerCard( everyone, Set(AdvancedResource)))))
  , RegularCard( "Consulting Gig"   , 7, Age2, Commercial, cost(""    ), Set(                   ), Set(GainFunding(2, PerCard( everyone, Set(AdvancedResource)))))
  , RegularCard( "Software Product" , 3, Age3, Commercial, cost("MFA" ), Set(                   ), Set(GainFunding(1, PerCard( myself, Set(BaseResource))),       perCard(1, myself, Set(BaseResource))))
  , RegularCard( "Software Product" , 4, Age3, Commercial, cost("MFA" ), Set(                   ), Set(GainFunding(1, PerCard( myself, Set(BaseResource))),       perCard(1, myself, Set(BaseResource))))
  , RegularCard( "Brand Recognition", 3, Age3, Commercial, cost("OY"  ), Set(                   ), Set(GainFunding(1, PerCard( myself, Set(Commercial))),         perCard(1, myself, Set(Commercial))))
  , RegularCard( "Brand Recognition", 6, Age3, Commercial, cost("OY"  ), Set(                   ), Set(GainFunding(1, PerCard( myself, Set(Commercial))),         perCard(1, myself, Set(Commercial))))
  , RegularCard( "Global Vision"    , 4, Age3, Commercial, cost("DDV" ), Set(                   ), Set(GainFunding(2, PerCard( myself, Set(AdvancedResource))),   perCard(2, myself, Set(AdvancedResource))))
  , RegularCard( "Global Vision"    , 6, Age3, Commercial, cost("DDV" ), Set(                   ), Set(GainFunding(2, PerCard( myself, Set(AdvancedResource))),   perCard(2, myself, Set(AdvancedResource))))
  , RegularCard( "Tech Conference"  , 3, Age3, Commercial, cost("OOF" ), Set(                   ), Set(GainFunding(3, ByStartupStage( myself)),                   AddVictory( CommercialVictory, 1, ByStartupStage(myself))))
  , RegularCard( "Tech Conference"  , 5, Age3, Commercial, cost("OOF" ), Set(                   ), Set(GainFunding(3, ByStartupStage( myself)),                   AddVictory( CommercialVictory, 1, ByStartupStage(myself))))
  , RegularCard( "Tech Conference"  , 7, Age3, Commercial, cost("OOF" ), Set(                   ), Set(GainFunding(3, ByStartupStage( myself)),                   AddVictory( CommercialVictory, 1, ByStartupStage(myself))))
  )
}
