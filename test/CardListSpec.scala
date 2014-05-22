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
      CardList.getResourceCard(CompanyProfile(Facebook, ASide), Project)  must beEqualTo(CompanyCard(CompanyProfile(Facebook, ASide), Project, Cost.empty, ProvideResource(Finance, 1, Shared)))
      CardList.getResourceCard(CompanyProfile(Twitter, ASide), Project)   must beEqualTo(CompanyCard(CompanyProfile(Twitter, ASide), Project, Cost.empty, ProvideResource(Youthfulness, 1, Shared)))
      CardList.getResourceCard(CompanyProfile(Apple, ASide), Project)     must beEqualTo(CompanyCard(CompanyProfile(Apple, ASide), Project, Cost.empty, ProvideResource(Vision, 1, Shared)))
      CardList.getResourceCard(CompanyProfile(Google, ASide), Project)    must beEqualTo(CompanyCard(CompanyProfile(Google, ASide), Project, Cost.empty, ProvideResource(Development, 1, Shared)))
      CardList.getResourceCard(CompanyProfile(Yahoo, ASide), Project)     must beEqualTo(CompanyCard(CompanyProfile(Yahoo, ASide), Project, Cost.empty, ProvideResource(Marketing, 1, Shared)))
      CardList.getResourceCard(CompanyProfile(Amazon, ASide), Project)    must beEqualTo(CompanyCard(CompanyProfile(Amazon, ASide), Project, Cost.empty, ProvideResource(Adoption, 1, Shared)))
      CardList.getResourceCard(CompanyProfile(Microsoft, ASide), Project) must beEqualTo(CompanyCard(CompanyProfile(Microsoft, ASide), Project, Cost.empty, ProvideResource(Operations, 1, Shared)))
    }

  }
}
