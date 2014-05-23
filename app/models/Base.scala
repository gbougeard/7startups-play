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
  object Facebook extends Company {override def toString = "Facebook"}
  object Twitter extends Company  {override def toString = "Twitter"}
  object Apple extends Company    {override def toString = "Apple"}
  object Google extends Company   {override def toString = "Google"}
  object Yahoo extends Company    {override def toString = "Yahoo"}
  object Amazon extends Company   {override def toString = "Amazon"}
  object Microsoft extends Company{override def toString = "Micro$oft"}

  lazy val companies: Set[Company] = Set(Facebook, Twitter, Apple, Google, Yahoo, Amazon, Microsoft)

  sealed trait CompanySide
  object ASide extends CompanySide {override def toString = "A"}
  object BSide extends CompanySide {override def toString = "B"}

  lazy val sides: Set[CompanySide] = Set(ASide, BSide)
  
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

  type Poacher = Int
  type VictoryPoint = Int
  type Funding = Int
  type PlayerCount = Int
  type Turn = Int

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
