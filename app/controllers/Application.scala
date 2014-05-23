package controllers

import play.api._
import play.api.mvc._
import models.Game

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def dealCompany(nbPlayer: Int) = Action {
    val deal = Game.dealCompanies(nbPlayer)
    Ok(views.html.companies(deal))
  }
}