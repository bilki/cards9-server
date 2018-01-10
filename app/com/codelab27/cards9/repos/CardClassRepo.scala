package com.codelab27.cards9.repos

import com.codelab27.cards9.models.cards.CardClass

trait CardClassRepo[F[_]] {

  def findCardClass(id: Option[CardClass.Id]): F[Option[CardClass]]

  def storeCardClass(cardClass: CardClass): F[Option[CardClass.Id]]

  def deleteCardClass(id: Option[CardClass.Id]): F[Option[CardClass]]

}