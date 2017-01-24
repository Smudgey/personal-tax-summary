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

import data.TaiTestData
import org.mockito.{Matchers, Mockito}
import org.scalatest.mock.MockitoSugar
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personaltaxsummary.connectors.AuthConnector
import uk.gov.hmrc.personaltaxsummary.controllers.action.{AccountAccessControl, AccountAccessControlWithHeaderCheck}
import uk.gov.hmrc.personaltaxsummary.domain.TaxSummaryContainer
import uk.gov.hmrc.personaltaxsummary.services.TaiService
import uk.gov.hmrc.personaltaxsummary.viewmodelfactories.TaxSummaryContainerFactory
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Future

trait Setup extends TaiTestData with MockitoSugar {
  implicit val hc = HeaderCarrier()

  val emptyRequest = FakeRequest()
  val mockTaiService: TaiService = mock[TaiService]
  val mockAuthConnector: AuthConnector = mock[AuthConnector]

  val currentYear = 2017
  val nino = Nino("CS700100A")

  val accountAccessControl: AccountAccessControl = new AccountAccessControl {
    override val authConnector: AuthConnector = mockAuthConnector
  }
  val accountAccessControlWithHeaderCheck: AccountAccessControlWithHeaderCheck = new AccountAccessControlWithHeaderCheck {
    override val accessControl: AccountAccessControl = accountAccessControl
  }
}

trait Success extends Setup {
  val controller = new PersonalTaxSummaryController {
    override val accessControl: AccountAccessControlWithHeaderCheck = accountAccessControlWithHeaderCheck
    override val service: TaiService = mockTaiService
  }

  val container: TaxSummaryContainer = TaxSummaryContainerFactory.createObject(nino, currentYearTaxSummary)
  Mockito.when(mockTaiService.getSummary(Matchers.eq(nino), Matchers.eq(currentYear))(Matchers.any(),Matchers.any())).thenReturn(Future.successful(Some(container)))
  Mockito.when(mockAuthConnector.grantAccess(Matchers.any())(Matchers.any(),Matchers.any())).thenReturn(Future.successful({}))
}
