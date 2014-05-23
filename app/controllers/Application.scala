package controllers

import play.api._
import play.api.mvc._
import models.Game
import models.Base._

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def dealCompany(nbPlayer: Int) = Action {
    val deal = Game.dealCompanies(nbPlayer)
    Ok(views.html.companies(deal))
  }

  def deal(age:Int, nbPlayer: Int) = Action {
    val a:Age = age match{
      case 1 => Age1
      case 2 => Age2
      case 3 => Age3
    }
    val deal = Game.deal(a, nbPlayer)
    Ok(views.html.deal(age, deal))
  }
}