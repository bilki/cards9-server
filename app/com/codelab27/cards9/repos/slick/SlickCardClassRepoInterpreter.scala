package com.codelab27.cards9.repos.slick

import java.net.URL

import com.codelab27.cards9.models.cards.CardClass
import com.codelab27.cards9.repos.CardClassRepo

import slick.dbio.{DBIO => SlickDBIO}
import slick.jdbc.{H2Profile, JdbcProfile}

trait SlickCardClassRepoInterpreter extends CardClassRepo[SlickDBIO] {

  protected val profile: JdbcProfile

  import profile.api._

  protected def db[T](ctx: T): Database

  implicit def cardClassId = MappedColumnType.base[CardClass.Id, Int](_.value, CardClass.Id.apply)

  implicit def cardClassName = MappedColumnType.base[CardClass.Name, String](_.value, CardClass.Name.apply)

  // Unchecked URL
  implicit def cardClassImg = MappedColumnType.base[URL, String](_.toString, new URL(_))

  private def cardClasses = TableQuery[CardClassTable]

  override def findCardClass(id: CardClass.Id): DBIO[Option[CardClass]] = {
    cardClasses.filter(_.id === id).result.headOption
  }

  override def storeCardClass(cardClass: CardClass): DBIO[CardClass.Id] = {
    cardClasses returning cardClasses.map(_.id) += cardClass
  }

  override def deleteCardClass(id: CardClass.Id): DBIO[Unit] = {
    cardClasses.filter(_.id === id).delete >> DBIO.successful(())
  }

  def createTable: DBIO[Unit] = cardClasses.schema.create

  private final class CardClassTable(tag: Tag) extends Table[CardClass](tag, "card_class") {

    def id = column[CardClass.Id]("id", O.PrimaryKey, O.AutoInc)

    def name = column[CardClass.Name]("name")

    def imgUrl = column[URL]("img")

    def * = (name, imgUrl, id.?) <> ((CardClass.apply _).tupled, CardClass.unapply)
  }
}

object H2CardClassRepoInterpreter extends SlickCardClassRepoInterpreter {

  override val profile = H2Profile

  import profile.backend.Database

  override def db[T](ctx: T) = Database.forURL(ctx.toString, driver = "org.h2.Driver")

}
