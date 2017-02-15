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

import java.util.UUID
import java.util.concurrent.TimeUnit

import data.TaiTestData
import org.mockito.{ArgumentCaptor, Matchers, Mockito}
import org.scalatest.mock.MockitoSugar
import play.api.libs.json.{JsValue, Json}
import play.api.test.FakeRequest
import play.api.test.Helpers.POST
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personaltaxsummary.connectors.{AccountWithLowCL, AuthConnector, FailToMatchTaxIdOnAuth, NinoNotFoundOnAccount}
import uk.gov.hmrc.personaltaxsummary.controllers.action.{AccountAccessControl, AccountAccessControlWithHeaderCheck}
import uk.gov.hmrc.personaltaxsummary.domain.{PersonalTaxSummaryContainer, TaxSummaryContainer}
import uk.gov.hmrc.personaltaxsummary.services.{LiveTaiService, TaiService}
import uk.gov.hmrc.personaltaxsummary.viewmodelfactories.{EstimatedIncomeViewModelFactory, TaxSummaryContainerFactory, YourTaxableIncomeViewModelFactory}
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Future

trait Setup extends TaiTestData with MockitoSugar {
  implicit val hc = HeaderCarrier()
  implicit val timeout = akka.util.Timeout(1000, TimeUnit.MILLISECONDS)

  val mockTaiService: TaiService = mock[TaiService]
  val mockAuthConnector: AuthConnector = mock[AuthConnector]
  val auditCaptor: ArgumentCaptor[Nino] = ArgumentCaptor.forClass(classOf[Nino])
  val yearCaptor: ArgumentCaptor[Int] = ArgumentCaptor.forClass(classOf[Int])

  val currentYear = 2017
  val journeyId: String = UUID.randomUUID().toString
  val nino = Nino("CS700100A")
  val ninoIncorrect = Nino("CS333100A")

  val someContainer: TaxSummaryContainer = TaxSummaryContainerFactory.createObject(nino, currentYearTaxSummary)
  val gatekeeperedContainer: TaxSummaryContainer = TaxSummaryContainerFactory.createObject(nino, gateKeeperUserTaxSummary)
  val currentYearTaxSummaryContainer = PersonalTaxSummaryContainer(currentYearTaxSummary, Map.empty)
  val estimatedIncomeViewModel = EstimatedIncomeViewModelFactory.createObject(nino, currentYearTaxSummaryContainer)
  val yourTaxableIncomeViewModel = YourTaxableIncomeViewModelFactory.createObject(nino, currentYearTaxSummaryContainer)

  val lowConfidence: JsValue = Json.parse("""{"code":"LOW_CONFIDENCE_LEVEL","message":"Confidence Level on account does not allow access"}""")
  val noNinoOnAccont: JsValue = Json.parse("""{"code":"UNAUTHORIZED","message":"NINO does not exist on account"}""")

  val emptyRequest = FakeRequest()
  val personalTaxSummaryContainerRequest: FakeRequest[JsValue] = FakeRequest(POST, "/some/url").withBody(Json.toJson(currentYearTaxSummaryContainer))
    .withHeaders("Content-Type" -> "application/json")
  val badRequest: FakeRequest[JsValue] = FakeRequest(POST, "/some/url").withBody(Json.toJson(someContainer))
    .withHeaders("Content-Type" -> "application/json")

  val accountAccessControl: AccountAccessControl = new AccountAccessControl {
    override val authConnector: AuthConnector = mockAuthConnector
  }
  val accountAccessControlWithHeaderCheck: AccountAccessControlWithHeaderCheck = new AccountAccessControlWithHeaderCheck {
    override val accessControl: AccountAccessControl = accountAccessControl
  }
  val personalTaxSummaryController = new PersonalTaxSummaryController {
    override val accessControl: AccountAccessControlWithHeaderCheck = accountAccessControlWithHeaderCheck
    override val service: TaiService = mockTaiService
  }
  val domainController = new PersonalTaxSummaryDomainController {
    override val service: TaiService = LiveTaiService
  }
}

trait Success extends Setup {
  Mockito.when(mockTaiService.getSummary(Matchers.eq(nino), Matchers.eq(currentYear))(Matchers.any(),Matchers.any())).thenReturn(Future.successful(Some(someContainer)))
  Mockito.when(mockAuthConnector.grantAccess(Matchers.any())(Matchers.any(),Matchers.any())).thenReturn(Future.successful({}))
}

trait AccessCheck extends Setup {
  Mockito.when(mockTaiService.getSummary(Matchers.any(), Matchers.eq(currentYear))(Matchers.any(),Matchers.any())).thenReturn(Future.successful(Some(someContainer)))
  Mockito.when(mockAuthConnector.grantAccess(Matchers.any())(Matchers.any(),Matchers.any())).thenReturn(Future.failed({new FailToMatchTaxIdOnAuth("BOOM!")}))
}

trait NoNino extends Setup {
  Mockito.when(mockTaiService.getSummary(Matchers.any(), Matchers.eq(currentYear))(Matchers.any(),Matchers.any())).thenReturn(Future.successful(Some(someContainer)))
  Mockito.when(mockAuthConnector.grantAccess(Matchers.any())(Matchers.any(),Matchers.any())).thenReturn(Future.failed({new NinoNotFoundOnAccount("NADA!")}))
}

trait AuthWithLowConfidence extends Setup {
  Mockito.when(mockTaiService.getSummary(Matchers.any(), Matchers.eq(currentYear))(Matchers.any(),Matchers.any())).thenReturn(Future.successful(Some(someContainer)))
  Mockito.when(mockAuthConnector.grantAccess(Matchers.any())(Matchers.any(),Matchers.any())).thenReturn(Future.failed({new AccountWithLowCL("TOO LOW!")}))
}

trait NotFound extends Setup {
  Mockito.when(mockTaiService.getSummary(Matchers.eq(nino), Matchers.eq(currentYear))(Matchers.any(),Matchers.any())).thenReturn(Future.successful(None))
  Mockito.when(mockAuthConnector.grantAccess(Matchers.any())(Matchers.any(),Matchers.any())).thenReturn(Future.successful({}))
}

trait GateKeepered extends Setup {
  Mockito.when(mockTaiService.getSummary(Matchers.eq(nino), Matchers.eq(currentYear))(Matchers.any(),Matchers.any())).thenReturn(Future.successful(Some(gatekeeperedContainer)))
  Mockito.when(mockAuthConnector.grantAccess(Matchers.any())(Matchers.any(),Matchers.any())).thenReturn(Future.successful({}))
}