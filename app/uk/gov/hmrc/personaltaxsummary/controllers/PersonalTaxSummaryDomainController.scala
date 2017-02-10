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

import jdk.nashorn.internal.ir.RuntimeNode.Request
import play.api.libs.json.{Writes, JsValue, JsError, Json}
import play.api.mvc.{Result, AnyContent, BodyParsers, Action}
import play.api.{mvc, Logger}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.model.TaxSummaryDetails
import uk.gov.hmrc.personaltaxsummary.services.{LiveTaiService, TaiService}
import uk.gov.hmrc.personaltaxsummary.viewmodels.{YourTaxableIncomeViewModel, EstimatedIncomeViewModel}
import uk.gov.hmrc.play.microservice.controller.BaseController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait PersonalTaxSummaryDomainController extends BaseController {

  val service: TaiService

  final def buildEstimatedIncome(nino: Nino, journeyId: Option[String] = None) = Action.async(BodyParsers.parse.json) {
    implicit request =>

      implicit val format = Json.format[EstimatedIncomeViewModel]
      buildDomain[EstimatedIncomeViewModel](nino,request) {
        nino => details => service.buildEstimatedIncome(nino, details)
      }
  }

  final def buildYourTaxableIncome(nino: Nino, journeyId: Option[String] = None) = Action.async(BodyParsers.parse.json) {
    implicit request =>

      implicit val format = Json.format[YourTaxableIncomeViewModel]
      buildDomain[YourTaxableIncomeViewModel](nino,request) {
        nino => details =>  service.buildYourTaxableIncome(nino, details)
      }
  }

  def buildDomain[T](nino:Nino,request:mvc.Request[JsValue])(func: => Nino => TaxSummaryDetails => T)(implicit tjs: Writes[T]) : Future[Result] = {
    request.body.validate[TaxSummaryDetails].fold(
      errors => {
        val failure = JsError.toJson(errors)
        Logger.warn("Received error with parsing tax summary details: " + failure)
        Future.successful(BadRequest(Json.obj("message" -> failure)))
      },
      taxSummary => {
        Future.successful(Ok(Json.toJson(func(nino)(taxSummary))))
      }
    )
  }

}

object PersonalTaxSummaryDomainController extends PersonalTaxSummaryDomainController {
  override val service: TaiService = LiveTaiService
}