package com.codelab27.cards9.models.players

import com.codelab27.cards9.models.boards.Board
import com.codelab27.cards9.models.cards.Fight

/**
  * A cards match.
  *
  * @param player1 player one identifier
  * @param player2 player two identifier
  * @param board the board
  * @param fights list of already computed fights
  */
final case class Match(
  player1: Player.Id, // Red player
  player2: Player.Id, // Blue player
  board: Board,
  fights: List[Fight] = Nil) {
}

object Match {

  case class Id(value: String) extends AnyVal

  case class RedScore(value: Int) extends AnyVal

  case class BlueScore(value: Int) extends AnyVal

  case class Score(red: RedScore, blue: BlueScore)

}
