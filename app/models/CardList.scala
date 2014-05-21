package models

import models.Base._

/**
 * Created by gbougeard on 21/05/14.
 */
object CardList {

  def getMaxStage(profile: CompanyProfile): CompanyStage = {
    profile match {
      case CompanyProfile(Facebook, BSide) => Stage2
      case CompanyProfile(Microsoft, BSide) => Stage4
      case _ => Stage3
    }
  }

  //  def getResourceCard(profile:CompanyProfile):CompanyStage


}
