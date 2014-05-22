import models.Base._
import models.Cards._
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
      Cards.cost("Y") must beEqualTo(Cost(Bag.from(Youthfullness -> 1), Funding(0)))
      Cards.cost("YV") must beEqualTo(Cost(Bag.from(Youthfullness -> 1, Vision -> 1), Funding(0)))
      Cards.cost("Y$") must beEqualTo(Cost(Bag.from(Youthfullness -> 1), Funding(1)))
      Cards.cost("YY") must beEqualTo(Cost(Bag.from(Youthfullness -> 2), Funding(0)))
    }

  }
}
