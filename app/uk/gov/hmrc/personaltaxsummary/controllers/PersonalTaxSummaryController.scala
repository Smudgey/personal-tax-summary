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

import play.api.libs.json.Json
import play.api.{Logger, mvc}
import uk.gov.hmrc.api.controllers._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personaltaxsummary.controllers.action.AccountAccessControlWithHeaderCheck
import uk.gov.hmrc.personaltaxsummary.services.{LiveTaiService, TaiService}
import uk.gov.hmrc.play.http._
import uk.gov.hmrc.play.microservice.controller.BaseController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait ErrorHandling {
  self: BaseController =>

  def errorWrapper(func: => Future[mvc.Result])(implicit hc: HeaderCarrier) = {
    func.recover {
      case ex: NotFoundException => Status(ErrorNotFound.httpStatusCode)(Json.toJson(ErrorNotFound))

      case ex: ServiceUnavailableException =>
        // The hod can return a 503 HTTP status which is translated to a 429 response code.
        // The 503 HTTP status code must only be returned from the API gateway and not from downstream API's.
        Logger.error(s"ServiceUnavailableException reported: ${ex.getMessage}", ex)
        Status(ClientRetryRequest.httpStatusCode)(Json.toJson(ClientRetryRequest))

      case e: Throwable =>
        Logger.error(s"Internal server error: ${e.getMessage}", e)
        Status(ErrorInternalServerError.httpStatusCode)(Json.toJson(ErrorInternalServerError))
    }
  }
}

trait PersonalTaxSummaryController extends BaseController with HeaderValidator with ErrorHandling {

  val service: TaiService
  val accessControl: AccountAccessControlWithHeaderCheck

  final def getSummary(nino: Nino, year: Int, journeyId: Option[String] = None) = accessControl.validateAcceptWithAuth(Some(nino)).async {
    implicit request =>
      implicit val hc = HeaderCarrier.fromHeadersAndSession(request.headers, None)
      errorWrapper(service.getSummary(nino, year).map {
        case Some(summary) => Ok(Json.toJson(summary))
        case _ => NotFound
      })
  }
}

object LivePersonalTaxSummaryController extends PersonalTaxSummaryController {
  override val service: TaiService = LiveTaiService
  override val accessControl: AccountAccessControlWithHeaderCheck = AccountAccessControlWithHeaderCheck
}