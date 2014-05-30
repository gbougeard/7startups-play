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

  def randomCommunities(nbPlayers: PlayerCount): List[RegularCard] = {
    Random.setSeed(new DateTime().getMillis)
    Random.shuffle(communities.toList).take(nbPlayers + 2)
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

  def addCommunities(age: Age, nbPlayers: PlayerCount): List[RegularCard] = {
    if (age == Age3) {
      randomCommunities(nbPlayers)
    }
    else List()
  }

  def deal(age: Age, nbPlayers: PlayerCount): Map[PlayerId, List[RegularCard]] = {

    def filterByAgeAndPlayerCount(c: RegularCard): Boolean = (c.cAge == age) && (c.cMinPlayers <= nbPlayers)
    def filterByAge(c: RegularCard): Boolean = c.cAge == age

    val regularCards = allCards.filter(filterByAgeAndPlayerCount) ++ addCommunities(age, nbPlayers)
    //    play.Logger.debug(s"total : ${allCards.size} byAge :  ${allCards.filter(filterByAge).size} filtered : ${allCards.filter(filterByAgeAndPlayerCount).size} communities : ${addCommunities(age, nbPlayers).size} => for $nbPlayers you have ${regularCards.size} cards")

    //    allCards.filter(filterByAgeAndPlayerCount).map(println)
    Random.setSeed(new DateTime().getMillis)
    val shuffleCards = Random.shuffle(regularCards.toList)
    val hands = shuffleCards.grouped(7).toList
    //    play.Logger.debug(s"hands $hands")
    playerList(nbPlayers).zip(hands).toMap[PlayerId, List[RegularCard]]

  }

  def playerList(nbPlayers: PlayerCount): Set[PlayerId] = {
    val players = for (i <- 1 to nbPlayers) yield s"Player$i"
    players.toSet
  }

  def init(nbPlayers: PlayerCount) = {
    val companies: Map[PlayerId, CompanyCard] = dealCompanies(nbPlayers)
    val players: Set[PlayerId] = playerList(nbPlayers)
    val age1Hand: Map[PlayerId, List[RegularCard]] = deal(Age1, nbPlayers)

    val age1PlayerStates = players.map(player => PlayerState(pCompany = companies.get(player).get.cCompany,
                                                             pCompanyStage = Project,
                                                             pCards = age1Hand.get(player).get,
                                                             pFunds = 3,
                                                             pNeighborhood = getNeighborhood(players.toList, player),
                                                             pPoachingResults = Set()))

  }

  def getNeighborhood(players : List[PlayerId], player:PlayerId): Neighborhood = {
    val rightIdx = players.indexOf(player) +1
    val leftIdx = players.indexOf(player) -1

    val right = if (rightIdx > players.size) players.head else players(rightIdx)
    val left = if (leftIdx < 0) players.last else players(leftIdx)
    (left, right)
  }

}
