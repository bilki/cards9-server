package models.cards

import org.scalatest._
import models.cards._
import models.ModelGens._
import specs.ModelSpec
import services.settings.GameSettings

class CardSpec extends ModelSpec {

  "A card" when {
    implicit val defaultGameSettings: GameSettings = new GameSettings {
      override val BOARD_SIZE: Int = 4
      override val BOARD_MAX_BLOCKS: Int = 6
      override val CARD_MAX_LEVEL: Int = 16
      override val MAX_HAND_CARDS: Int = 5
    }

    "created" should {
      "have correct power level according to game settings" in {
        forAll { card: Card =>
          card.power should be < defaultGameSettings.CARD_MAX_LEVEL
        }
      }

      "have correct physical defense level according to game settings" in {
        forAll { card: Card =>
          card.pdef should be < defaultGameSettings.CARD_MAX_LEVEL
        }
      }

      "have correct magic defense level according to game settings" in {
        forAll { card: Card =>
          card.mdef should be < defaultGameSettings.CARD_MAX_LEVEL
        }
      }

      "have a number of arrows less or equal than max arrows" in {
        forAll { card: Card =>
          card.arrows.size should be <= Arrow.MAX_ARROWS
        }
      }

      "have a list of distinct arrows" in {
        forAll { card: Card =>
          card.arrows.distinct should be(card.arrows)
        }
      }
    }
  }

}
