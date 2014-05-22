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
                         pCards: Set[Card],
                         pFunds: Funding,
                         pNeighborhodd: Neighborhood,
                         pPoachingResults: Set[PoachingOutcome])

  case class GameState(playerMap: Map[PlayerId, PlayerState],
                       discardPile: Set[Card],
                       rnd: Random)

  sealed trait ActionType
  object Play extends ActionType
  object Drop extends ActionType
  object BuildCompany extends ActionType

  case class PlayerAction(actionType: ActionType, card: Card)

//  type NonInteractive = MonadState[_, GameState]
//  type GameStateOnly = MonadState[_, GameState]

  sealed trait Communication
  case class RawMessage(msg: String) extends Communication
  case class ActionRecapMsg(gameState: GameState, exchanges: Map[PlayerId, (PlayerAction, Exchange)]) extends Communication

  sealed trait CommunicationType
  case class PlayerCom(playerId: PlayerId, com: Communication) extends CommunicationType
  case class BroasdcastCom(com: Communication) extends CommunicationType

}
