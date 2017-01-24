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

package uk.gov.hmrc.personaltaxsummary.controllers.action

import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc.{ActionBuilder, Request, Result, Results}
import uk.gov.hmrc.api.controllers.{ErrorResponse, ErrorUnauthorizedLowCL, HeaderValidator}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personaltaxsummary.connectors._
import uk.gov.hmrc.personaltaxsummary.controllers.ErrorUnauthorizedNoNino
import uk.gov.hmrc.play.auth.microservice.connectors.ConfidenceLevel
import uk.gov.hmrc.play.http.hooks.HttpHook
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpGet, HttpResponse}

import scala.concurrent.Future

case object ErrorUnauthorizedMicroService extends ErrorResponse(401, "UNAUTHORIZED", "Unauthorized to access resource")

case object ErrorUnauthorizedWeakCredStrength extends ErrorResponse(401, "WEAK_CRED_STRENGTH", "Credential Strength on account does not allow access")

trait AccountAccessControl extends Results {

  import scala.concurrent.ExecutionContext.Implicits.global

  val authConnector: AuthConnector

  case object ErrorUnauthorized extends ErrorResponse(401, "UNAUTHORIZED", "Invalid request")

  def invokeAuthBlock[A](request: Request[A], block: (Request[A]) => Future[Result], taxId: Option[Nino]) = {
    implicit val hc = HeaderCarrier.fromHeadersAndSession(request.headers, None)

    authConnector.grantAccess(taxId).flatMap { access =>
      block(request)
    }.recover {
      case ex: uk.gov.hmrc.play.http.Upstream4xxResponse =>
        Logger.info("Unauthorized! Failed to grant access since 4xx response!")
        Unauthorized(Json.toJson(ErrorUnauthorizedMicroService))

      case ex: NinoNotFoundOnAccount =>
        Logger.info("Unauthorized! NINO not found on account!")
        Unauthorized(Json.toJson(ErrorUnauthorizedNoNino))

      case ex: FailToMatchTaxIdOnAuth =>
        Logger.info("Unauthorized! Failure to match URL NINO against Auth NINO")
        Status(ErrorUnauthorized.httpStatusCode)(Json.toJson(ErrorUnauthorized))

      case ex: AccountWithLowCL =>
        Logger.info("Unauthorized! Account with low CL!")
        Unauthorized(Json.toJson(ErrorUnauthorizedLowCL))

      case ex: AccountWithWeakCredStrength =>
        Logger.info("Unauthorized! Account with weak cred strength!")
        Unauthorized(Json.toJson(ErrorUnauthorizedWeakCredStrength))
    }
  }

}

trait AccountAccessControlWithHeaderCheck extends HeaderValidator {
  val checkAccess = true
  val accessControl: AccountAccessControl

  def validateAcceptWithAuth(taxId: Option[Nino]) = new ActionBuilder[Request] {

    def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]): Future[Result] = {
      if (checkAccess) accessControl.invokeAuthBlock(request, block, taxId)
      else block(request)
    }
  }
}

object Auth {
  val authConnector: AuthConnector = AuthConnector
}

object AccountAccessControl extends AccountAccessControl {
  val authConnector: AuthConnector = Auth.authConnector
}

object AccountAccessControlWithHeaderCheck extends AccountAccessControlWithHeaderCheck {
  val accessControl: AccountAccessControl = AccountAccessControl
}

object AccountAccessControlOff extends AccountAccessControl {
  val authConnector: AuthConnector = new AuthConnector {
    override val serviceUrl: String = "NO SERVICE"

    override def serviceConfidenceLevel: ConfidenceLevel = ConfidenceLevel.L0

    override def http: HttpGet = new HttpGet {
      override protected def doGet(url: String)(implicit hc: HeaderCarrier): Future[HttpResponse] = Future.failed(new IllegalArgumentException("Sandbox mode!"))

      override val hooks: Seq[HttpHook] = NoneRequired
    }
  }
}

object AccountAccessControlCheckOff extends AccountAccessControlWithHeaderCheck {
  override val checkAccess = false

  val accessControl: AccountAccessControl = AccountAccessControlOff
}