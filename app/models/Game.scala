package models

import models.Base._
import models.Cards._
import models.CardList._
import org.scalacheck.Gen
import models.GameTypes._
import scala.util.Random
import org.joda.time.DateTime

/**
 * Created by gbougeard on 23/05/14.
 */
object Game {

  def randomCommunities(nbPlayers: PlayerCount): Set[RegularCard] = {
    Random.setSeed(new DateTime().getMillis)
    Random.shuffle(communities).take(nbPlayers + 2)
  }

  def randomCompanies(nbPlayers: PlayerCount): Set[Company] = {
    Random.setSeed(new DateTime().getMillis)
    Random.shuffle(companies).take(nbPlayers)
  }

  def randomSide: CompanySide = Gen.pick(1, sides).sample.get.head


  def dealCompanies(nbPlayers: PlayerCount): Map[PlayerId, CompanyCard] = {
    val profiles: Set[CompanyCard] = randomCompanies(nbPlayers).map(c => getResourceCard(CompanyProfile(c, randomSide), Project))
    playerList(nbPlayers).zip(profiles).toMap[PlayerId, CompanyCard]
  }

  def addCommunities(age: Age, nbPlayers: PlayerCount): Set[RegularCard] = {
    if (age == Age3) {
      randomCommunities(nbPlayers)
    }
    else Set()
  }

  def deal(age: Age, nbPlayers: PlayerCount): Map[PlayerId, Set[RegularCard]] = {

    def filterByAgeAndPlayerCount(c: RegularCard): Boolean = (c.cAge == age) && (c.cMinPlayers >= nbPlayers)

    val regularCards = allCards.filter(filterByAgeAndPlayerCount) ++ addCommunities(age, nbPlayers)

    val hands = Random.shuffle(regularCards).grouped(7).toList
    playerList(nbPlayers).zip(hands).toMap[PlayerId, Set[RegularCard]]

  }

  def playerList(nbPlayers: PlayerCount): Set[PlayerId] = {
    val players = for (i <- 1 to nbPlayers) yield s"Player$i"
    players.toSet
  }

}
