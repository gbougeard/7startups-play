import models.Base._
import models.Cards._
import models.Game
import org.specs2.mutable._


/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
class GameSpec extends Specification {

  "Game" should {

    "get random(nbPlayer + 2) communities" in {
      val coms: List[RegularCard] = Game.addCommunities(Age3, 5)
      val coms2: List[RegularCard] = Game.addCommunities(Age3, 5)
      //      coms.map(x => println(x.cName))
      //      println("===")
      //      coms2.map(x => println(x.cName))
      coms.size must beEqualTo(7)
      coms mustNotEqual coms2
    }

    "get no communities if not Age3" in {
      val coms: List[RegularCard] = Game.addCommunities(Age2, 5)
      coms must beEmpty
    }


    "deal cards " in {
      val cards = Game.deal(Age1, 5)
      //      println(s"$cards")
      cards.size must beEqualTo(5)
    }


    "deal companies " in {
      val cards = Game.dealCompanies(7)
//      cards.keys.map(k => println(s"$k - ${cards(k).cCompany}"))
      cards.size mustEqual 7
    }


  }
}
