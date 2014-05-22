package models

/**
 * Created by gbougeard on 21/05/14.
 */
object Base {

  sealed trait Age
  object Age1 extends Age
  object Age2 extends Age
  object Age3 extends Age

  sealed trait CompanyStage
  object Project extends CompanyStage
  object Stage1 extends CompanyStage
  object Stage2 extends CompanyStage
  object Stage3 extends CompanyStage
  object Stage4 extends CompanyStage

  sealed trait Company
  object Facebook extends Company
  object Twitter extends Company
  object Apple extends Company
  object Google extends Company
  object Yahoo extends Company
  object Amazon extends Company
  object Microsoft extends Company

  sealed trait CompanySide
  object ASide extends CompanySide
  object BSide extends CompanySide
  
  case class CompanyProfile(company:Company, side:CompanySide)

  sealed trait Resource
  sealed trait BaseResource extends Resource
  sealed trait AdvancedResource extends Resource
  object Youthfulness extends AdvancedResource
  object Vision extends AdvancedResource
  object Adoption extends AdvancedResource
  object Development extends BaseResource
  object Operations extends BaseResource
  object Marketing extends BaseResource
  object Finance extends BaseResource
  
  val baseResources:Set[Resource] = Set(Development, Operations, Marketing, Finance) 
  val advancedResources:Set[Resource] = Set(Adoption, Vision, Youthfulness) 

  case class Poacher(value: Int)

  case class VictoryPoint(value: Int)

  case class Funding(value: Int)

  case class PlayerCount(value: Int)

  sealed case class Turn(value: Int)

  sealed trait PoachingOutcome
  object Defeat extends PoachingOutcome
  object Victory extends PoachingOutcome with Age


  sealed trait VictoryType
  object PoachingVictory extends VictoryType
  object FundingVictory extends VictoryType
  object CompanyVictory extends VictoryType
  object InfrastructureVictory extends VictoryType
  object RnDVictory extends VictoryType
  object CommercialVictory extends VictoryType
  object CommunityVictory extends VictoryType


}
