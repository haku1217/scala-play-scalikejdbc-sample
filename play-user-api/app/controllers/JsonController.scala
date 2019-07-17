package controllers

import javax.inject.Inject

import play.api.mvc._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import scalikejdbc._
import models._

import JsonController._


object JsonController {

  implicit val usersWrites = (
      (__ \ "id"       ).write[Long]   and
      (__ \ "name"     ).write[String] and
      (__ \ "companyId").writeNullable[Int]
    )(unlift(Users.unapply))

  case class UserForm(id: Option[Long], name: String, companyId: Option[Int])

  implicit val userFormReads = (
      (__ \ "id"       ).readNullable[Long] and
      (__ \ "name"     ).read[String] and
      (__ \ "companyId").readNullable[Int]
  )(UserForm)

}

class JsonController @Inject()(components: ControllerComponents)
  extends AbstractController(components) {

  def list = Action { implicit request =>
    val u = Users.syntax("u")

    DB.readOnly { implicit session =>
      // ユーザのリストを取得
      val users = withSQL {
        select.from(Users as u).orderBy(u.id.asc)
      }.map(Users(u.resultName)).list.apply()

      // ユーザの一覧をJSONで返す
      Ok(Json.obj("users" -> users))
    }
  }

  def create = Action(parse.json) { implicit request =>
    request.body.validate[UserForm].map { form =>
      DB.localTx {implicit session =>
        Users.create(form.name, form.companyId)
        Ok(Json.obj("result" -> "success"))
      }
    }.recoverTotal { e =>
      BadRequest(Json.obj("result" -> "failure", "error" -> JsError.toJson(e)))
    }
  }

  def update = Action(parse.json) { implicit request =>
    request.body.validate[UserForm].map { form =>
      DB.localTx { implicit session =>
        Users.find(form.id.get).foreach { user =>
          Users.save(user.copy(name = form.name, companyId = form.companyId))
        }
        Ok(Json.obj("result" -> "success"))
      }
    }.recoverTotal { e =>
      BadRequest(Json.obj("result" -> "failure", "error" -> JsError.toJson(e)))
    }
  }

  def remove(id: Long) = Action { implicit request =>
    DB.localTx { implicit session =>
      // ユーザを削除
      Users.find(id).foreach { user =>
        Users.destroy(user)
      }
      Ok(Json.obj("result" -> "success"))
    }
  }

}