package com.codelab27.cards9.repos.slick

import java.net.URL

import com.codelab27.cards9.models.cards.CardClass
import com.codelab27.cards9.repos.CardClassRepo

import slick.dbio.{DBIO => SlickDBIO}
import slick.jdbc.{H2Profile, JdbcProfile}

import scala.concurrent.ExecutionContext.Implicits.global

abstract class SlickCardClassRepoInterpreter(val profile: JdbcProfile) extends CardClassRepo[SlickDBIO] {

  import profile.api._

  protected val driver: String

  def db: Database = Database
    .forURL("jdbc:h2:mem:play;MODE=PostgreSQL;DB_CLOSE_DELAY=-1;DATABASE_TO_UPPER=FALSE", driver = driver)

  implicit def cardClassId = MappedColumnType.base[CardClass.Id, Int](_.value, CardClass.Id.apply)

  implicit def cardClassName = MappedColumnType.base[CardClass.Name, String](_.value, CardClass.Name.apply)

  // Unchecked URL
  implicit def cardClassImg = MappedColumnType.base[URL, String](_.toString, new URL(_))

  private def cardClasses = TableQuery[CardClassTable]

  override def findCardClass(id: Option[CardClass.Id]): DBIO[Option[CardClass]] = {
    cardClasses.filter(_.id === id).result.headOption
  }

  override def storeCardClass(cardClass: CardClass): DBIO[Option[CardClass.Id]] = {
    cardClasses returning cardClasses.map(_.id) insertOrUpdate(cardClass)
  }

  override def deleteCardClass(id: Option[CardClass.Id]): DBIO[Option[CardClass]] = {
    (for {
      cardClass <- findCardClass(id)
      _         <- cardClasses.filter(_.id === id).delete
    } yield {
      cardClass
    }).transactionally
  }

  def createTable: DBIO[Unit] = cardClasses.schema.create

  private final class CardClassTable(tag: Tag) extends Table[CardClass](tag, "card_class") {

    def id = column[CardClass.Id]("id", O.PrimaryKey, O.AutoInc)

    def name = column[CardClass.Name]("name")

    def imgUrl = column[URL]("img")

    def * = (name, imgUrl, id.?) <> ((CardClass.apply _).tupled, CardClass.unapply)
  }
}

object H2CardClassRepoInterpreter extends SlickCardClassRepoInterpreter(H2Profile) {
  override protected val driver = "org.h2.Driver"
}
