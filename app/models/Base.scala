package models

/**
 * Created by gbougeard on 21/05/14.
 */
object Base {

  sealed trait Age
  case object Age1 extends Age
  case object Age2 extends Age
  case object Age3 extends Age

  sealed trait CompanyStage
  case object Project extends CompanyStage
  case object Stage1 extends CompanyStage
  case object Stage2 extends CompanyStage
  case object Stage3 extends CompanyStage
  case object Stage4 extends CompanyStage

  sealed trait Company
  case object Facebook extends Company
  case object Twitter extends Company
  case object Apple extends Company
  case object Google extends Company
  case object Yahoo extends Company
  case object Amazon extends Company
  case object Microsoft extends Company

  lazy val companies: Set[Company] = Set(Facebook, Twitter, Apple, Google, Yahoo, Amazon, Microsoft)

  sealed trait CompanySide
  case object ASide extends CompanySide
  case object BSide extends CompanySide

  lazy val sides: Set[CompanySide] = Set(ASide, BSide)
  
  case class CompanyProfile(company:Company, side:CompanySide)

  sealed trait Resource
  sealed trait BaseResource extends Resource
  sealed trait AdvancedResource extends Resource
  case object Youthfulness extends AdvancedResource
  case object Vision extends AdvancedResource
  case object Adoption extends AdvancedResource
  case object Development extends BaseResource
  case object Operations extends BaseResource
  case object Marketing extends BaseResource
  case object Finance extends BaseResource
  
  val baseResources:Set[Resource] = Set(Development, Operations, Marketing, Finance) 
  val advancedResources:Set[Resource] = Set(Adoption, Vision, Youthfulness) 

  type Poacher = Int
  type VictoryPoint = Int
  type Funding = Int
  type PlayerCount = Int
  type Turn = Int

  sealed trait PoachingOutcome
  case object Defeat extends PoachingOutcome
  case object Victory extends PoachingOutcome with Age

  sealed trait VictoryType
  case object PoachingVictory extends VictoryType
  case object FundingVictory extends VictoryType
  case object CompanyVictory extends VictoryType
  case object InfrastructureVictory extends VictoryType
  case object RnDVictory extends VictoryType
  case object CommercialVictory extends VictoryType
  case object CommunityVictory extends VictoryType


}
