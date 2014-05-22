import models.Base._
import models.Base.CompanyProfile
import models.Base.Funding
import models.Cards._
import models.Cards.ProvideResource
import models.{Cards, CardList}
import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._


import play.api.test._
import play.api.test.Helpers._
import scala.collection.Bag


/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
class CardListSpec extends Specification {

  "CardList" should {

    "return Stage2 when asking max stage for Facebook BSide" in {
      CardList.getMaxStage(CompanyProfile(Facebook, BSide)) must beEqualTo(Stage2)
    }

    "return Stage4 when asking max stage for Microsoft BSide" in {
      CardList.getMaxStage(CompanyProfile(Microsoft, BSide)) must beEqualTo(Stage4)
    }

    "return Stage3 when asking max stage for others" in {
      CardList.getMaxStage(CompanyProfile(Microsoft, ASide)) must beEqualTo(Stage3)
      CardList.getMaxStage(CompanyProfile(Facebook, ASide)) must beEqualTo(Stage3)
      CardList.getMaxStage(CompanyProfile(Twitter, ASide)) must beEqualTo(Stage3)
      CardList.getMaxStage(CompanyProfile(Twitter, BSide)) must beEqualTo(Stage3)
    }

    "calculate the right cost" in {
      Cards.cost("Y")  must beEqualTo(Cost(Bag.from(Youthfulness -> 1), Funding(0)))
      Cards.cost("YV") must beEqualTo(Cost(Bag.from(Youthfulness -> 1, Vision -> 1), Funding(0)))
      Cards.cost("Y$") must beEqualTo(Cost(Bag.from(Youthfulness -> 1), Funding(1)))
      Cards.cost("YY") must beEqualTo(Cost(Bag.from(Youthfulness -> 2), Funding(0)))
    }

    "getResourceCard for A Side at Project stage" in {
      val side = ASide
      val stage = Project
      CardList.getResourceCard(CompanyProfile(Facebook, side), stage)  must beEqualTo(CompanyCard(CompanyProfile(Facebook, side), stage, Cost.empty, Set(ProvideResource(Finance, 1, Shared))))
      CardList.getResourceCard(CompanyProfile(Twitter, side), stage)   must beEqualTo(CompanyCard(CompanyProfile(Twitter, side), stage, Cost.empty, Set(ProvideResource(Youthfulness, 1, Shared))))
      CardList.getResourceCard(CompanyProfile(Apple, side), stage)     must beEqualTo(CompanyCard(CompanyProfile(Apple, side), stage, Cost.empty, Set(ProvideResource(Vision, 1, Shared))))
      CardList.getResourceCard(CompanyProfile(Google, side), stage)    must beEqualTo(CompanyCard(CompanyProfile(Google, side), stage, Cost.empty, Set(ProvideResource(Development, 1, Shared))))
      CardList.getResourceCard(CompanyProfile(Yahoo, side), stage)     must beEqualTo(CompanyCard(CompanyProfile(Yahoo, side), stage, Cost.empty, Set(ProvideResource(Marketing, 1, Shared))))
      CardList.getResourceCard(CompanyProfile(Amazon, side), stage)    must beEqualTo(CompanyCard(CompanyProfile(Amazon, side), stage, Cost.empty, Set(ProvideResource(Adoption, 1, Shared))))
      CardList.getResourceCard(CompanyProfile(Microsoft, side), stage) must beEqualTo(CompanyCard(CompanyProfile(Microsoft, side), stage, Cost.empty, Set(ProvideResource(Operations, 1, Shared))))
    }

  }
}
