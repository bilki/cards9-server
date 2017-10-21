package com.codelab27.cards9.utils

sealed trait GameError

case class FightError(msg: String) extends GameError
