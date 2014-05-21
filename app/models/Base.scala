package models

/**
 * Created by gbougeard on 21/05/14.
 */
object Base {

  sealed trait Age

  sealed trait Age1 extends Age

  sealed trait Age2 extends Age

  sealed trait Age3 extends Age

  sealed trait CompanyStage

  sealed trait Project extends CompanyStage

  sealed trait Stage1 extends CompanyStage

  sealed trait Stage2 extends CompanyStage

  sealed trait Stage3 extends CompanyStage

  sealed trait Stage4 extends CompanyStage

  sealed trait Company

  sealed trait Facebook extends Company

  sealed trait Twitter extends Company

  sealed trait Apple extends Company

  sealed trait Google extends Company

  sealed trait Yahoo extends Company

  sealed trait Amazon extends Company

  sealed trait Microsoft extends Company

  sealed trait CompanySide

  sealed trait ASide extends CompanySide

  sealed trait BSide extends CompanySide
  
  case class CompanyProfile(profile:CompanyProfile, company:Company, side:CompanySide)
  

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

  sealed case class Poacher(value: Int)

  sealed case class VictoryPoint(value: Int)

  sealed case class Funding(value: Int)

  sealed case class PlayerCount(value: Int)

  sealed case class Turn(value: Int)

  sealed trait PoachingOutcome
  sealed trait Defeat extends PoachingOutcome
  sealed trait Victory extends PoachingOutcome with Age


  sealed trait VictoryType

  sealed trait PoachingVictory extends VictoryType

  sealed trait FundingVictory extends VictoryType

  sealed trait CompanyVictory extends VictoryType

  sealed trait InfrastructureVictory extends VictoryType

  sealed trait RnDVictory extends VictoryType

  sealed trait CommercialVictory extends VictoryType

  sealed trait CommunityVictory extends VictoryType


}
