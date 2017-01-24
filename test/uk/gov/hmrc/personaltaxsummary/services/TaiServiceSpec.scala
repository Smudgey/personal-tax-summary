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

import org.mockito.Matchers.any
import org.mockito.Mockito.verify
import org.mockito.{ArgumentCaptor, Mockito}
import uk.gov.hmrc.personaltaxsummary.domain.TaxSummaryContainer
import uk.gov.hmrc.play.audit.model.DataEvent
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

import scala.concurrent.ExecutionContext.Implicits.global

class TaiServiceSpec extends UnitSpec with WithFakeApplication with Setup {

  "TaiService getSummary" should {
    "return a tax summary container given a valid NINO and year" in {
      val result: Option[TaxSummaryContainer] = await(TaiServiceTest.getSummary(nino, currentYear))

      result.isDefined shouldBe true
    }

    "not return a tax summary when no summary is found" in {
      val result: Option[TaxSummaryContainer] = await(TaiServiceTest.getSummary(nino, distantFuture))

      result.isEmpty shouldBe true
    }

    "log an audit message" in {
      await(TaiServiceTest.getSummary(nino, currentYear))

      val auditCaptor = ArgumentCaptor.forClass(classOf[DataEvent])
      verify(mockAuditConnector, Mockito.atLeast(1)).sendEvent(auditCaptor.capture())(any(),any())

      val actual: DataEvent = auditCaptor.getValue

      actual.auditSource shouldBe "personal-tax-summary"
      actual.tags.get("transactionName") shouldBe Some("getSummary")
      actual.detail.get("nino") shouldBe Some(nino.nino)
      actual.detail.get("year") shouldBe Some(currentYear.toString)
    }
  }
}