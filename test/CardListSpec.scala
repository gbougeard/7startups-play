import models.Base._
import models.Cards._
import models.CardList
import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._


import play.api.test._
import play.api.test.Helpers._


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

  }
}
