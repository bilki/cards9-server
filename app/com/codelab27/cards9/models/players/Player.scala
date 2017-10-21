package com.codelab27.cards9.models.players

import com.codelab27.cards9.models.cards.Card

/**
  * A game player.
  *
  * @param id unique identifier
  * @param cards cards owned by the player
  */
final case class Player(
  id: Player.Id,
  cards: List[Card.Id])

object Player {

  case class Id(value: Int) extends AnyVal

}
