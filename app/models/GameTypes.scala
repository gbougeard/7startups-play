package models

import models.Base._
import models.Cards._
import scala.util.Random
import scalaz.MonadState

/**
 * Created by gbougeard on 22/05/14.
 */
object GameTypes {

  type PlayerId = String
  type Neighborhood = (PlayerId, PlayerId)
  type Message = String

  case class PlayerState(pCompany: CompanyProfile,
                         pCompanyStage: CompanyStage,
                         pCards: List[RegularCard],
                         pFunds: Funding,
                         pNeighborhood: Neighborhood,
                         pPoachingResults: Set[PoachingOutcome])

  case class GameState(playerMap: Map[PlayerId, PlayerState],
                       discardPile: List[RegularCard],
                       rnd: Random)

  sealed trait ActionType
  case object Play extends ActionType
  case object Drop extends ActionType
  case object BuildCompany extends ActionType

  case class PlayerAction(actionType: ActionType, card: RegularCard)

  type NonInteractive = MonadState[GameState, Int]
  type GameStateOnly = MonadState[GameState, Int]

  sealed trait Communication
  case class RawMessage(msg: String) extends Communication
  case class ActionRecapMsg(gameState: GameState, exchanges: Map[PlayerId, (PlayerAction, Exchange)]) extends Communication

  sealed trait CommunicationType
  case class PlayerCom(playerId: PlayerId, com: Communication) extends CommunicationType
  case class BroadcastCom(com: Communication) extends CommunicationType

}
