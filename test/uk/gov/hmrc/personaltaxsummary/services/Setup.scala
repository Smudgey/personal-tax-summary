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

package uk.gov.hmrc.personaltaxsummary.services

import data.TaiTestData
import org.mockito.Matchers.any
import org.mockito.{Matchers, Mockito}
import org.scalatest.mock.MockitoSugar
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personaltaxsummary.connectors.TaiConnector
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Future

trait Setup extends TaiTestData with MockitoSugar {
  implicit val hc = HeaderCarrier()

  val mockTaiConnector: TaiConnector = mock[TaiConnector]

  val nino = Nino("KM569110B")
  val nonCoded = Nino("CZ629113A")
  val gateKeepered = Nino("CS700100A")
  val currentYear = 1

  object TaiServiceTest extends TaiService {
    override val taiConnector: TaiConnector = mockTaiConnector
  }

  Mockito.when(mockTaiConnector.taxSummary(Matchers.eq(nino), Matchers.eq(currentYear))(any(), any())).thenReturn(Future.successful(Option(currentYearTaxSummary)))
  Mockito.when(mockTaiConnector.taxSummary(Matchers.eq(nonCoded), Matchers.eq(currentYear))(any(), any())).thenReturn(Future.successful(Option(nonCodedTaxSummary)))
  Mockito.when(mockTaiConnector.taxSummary(Matchers.eq(gateKeepered), Matchers.eq(currentYear))(any(), any())).thenReturn(Future.successful(Option(gateKeeperUserTaxSummary)))
}