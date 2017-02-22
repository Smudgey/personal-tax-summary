/*
 * Copyright 2017 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.personaltaxsummary.controllers

import play.api.libs.json._
import play.api.mvc.{Action, BodyParsers, Result}
import play.api.{Logger, mvc}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personaltaxsummary.domain.PersonalTaxSummaryContainer
import uk.gov.hmrc.personaltaxsummary.services.PersonalTaxSummaryDomainFactory
import uk.gov.hmrc.play.microservice.controller.BaseController

import scala.concurrent.Future

trait PersonalTaxSummaryDomainController extends BaseController {

  val domain: PersonalTaxSummaryDomainFactory

  final def buildEstimatedIncome(nino: Nino, journeyId: Option[String] = None) = Action.async(BodyParsers.parse.json) {
    implicit request =>

      buildDomain(nino,request) {
        nino => container => domain.buildEstimatedIncome(nino, container)
      }
  }

  final def buildYourTaxableIncome(nino: Nino, journeyId: Option[String] = None) = Action.async(BodyParsers.parse.json) {
    implicit request =>

      buildDomain(nino,request) {
        nino => container =>  domain.buildYourTaxableIncome(nino, container)
      }
  }

  def buildDomain[T](nino:Nino,request:mvc.Request[JsValue])(func: => Nino => PersonalTaxSummaryContainer => T)(implicit tjs: Writes[T]) : Future[Result] = {
    request.body.validate[PersonalTaxSummaryContainer].fold(
      errors => {
        val failure = JsError.toJson(errors)
        Logger.warn("Received error with parsing container: " + failure)
        Future.successful(BadRequest(Json.obj("message" -> failure)))
      },
      container => {
        Future.successful(Ok(Json.toJson(func(nino)(container))))
      }
    )
  }

}

object PersonalTaxSummaryDomainController extends PersonalTaxSummaryDomainController {
  override val domain = PersonalTaxSummaryDomainFactory
}