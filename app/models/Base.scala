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

  sealed trait Project extends CompanyStage

  object Stage1 extends CompanyStage

  object Stage2 extends CompanyStage

  object Stage3 extends CompanyStage

  object Stage4 extends CompanyStage

  sealed trait Company

  object Facebook extends Company

  object Twitter extends Company

  sealed trait Apple extends Company

  sealed trait Google extends Company

  sealed trait Yahoo extends Company

  sealed trait Amazon extends Company

  object Microsoft extends Company

  sealed trait CompanySide

  object ASide extends CompanySide

  object BSide extends CompanySide
  
  case class CompanyProfile(company:Company, side:CompanySide)
  

  sealed trait Resource

  sealed trait BaseResource extends Resource

  sealed trait AdvancedResource extends Resource

  sealed trait Youthfullness extends AdvancedResource

  sealed trait Vision extends AdvancedResource

  sealed trait Adoption extends AdvancedResource

  sealed trait Development extends BaseResource

  sealed trait Operations extends BaseResource

  sealed trait Marketing extends BaseResource

  sealed trait Finance extends BaseResource

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
