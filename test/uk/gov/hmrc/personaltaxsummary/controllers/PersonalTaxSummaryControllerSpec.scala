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

import org.mockito.Matchers.any
import org.mockito.Mockito.verify
import org.scalatest.concurrent.ScalaFutures
import play.api.libs.json.Json
import play.api.test.FakeApplication
import play.api.test.Helpers.contentAsJson
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personaltaxsummary.config.StubApplicationConfiguration
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

class PersonalTaxSummaryControllerSpec  extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration {

  override lazy val fakeApplication = FakeApplication(additionalConfiguration = config)

  "getSummary Live" should {

    "return a summary" in new Success {

      val result = await(controller.getSummary(nino, currentYear)(emptyRequest))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(someContainer)
    }

    "process the authentication successfully when journeyId is supplied" in new Success {

      val result = await(controller.getSummary(nino, currentYear, Some(journeyId))(emptyRequest))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(someContainer)
    }

    "return the gate keeper details given a gatekeepered user" in new GateKeepered {

      val result = await(controller.getSummary(nino, currentYear, Some(journeyId))(emptyRequest))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(gatekeeperedContainer)

      verify(mockTaiService).getSummary(auditCaptor.capture(), yearCaptor.capture())(any(),any())
      val actualNino: Nino = auditCaptor.getValue
      val actualYear: Int = yearCaptor.getValue

      actualNino shouldBe nino
      actualYear shouldBe currentYear
    }

    "return not found when summary returned is None" in new NotFound {

      val result = await(controller.getSummary(nino, currentYear)(emptyRequest))

      status(result) shouldBe 404

      verify(mockTaiService).getSummary(auditCaptor.capture(), yearCaptor.capture())(any(),any())
      val actualNino: Nino = auditCaptor.getValue
      val actualYear: Int = yearCaptor.getValue

      actualNino shouldBe nino
      actualYear shouldBe currentYear
    }

    "return unauthorized when the nino in the request does not match the authority nino" in new AccessCheck {

      val result = await(controller.getSummary(ninoIncorrect, currentYear)(emptyRequest))

      status(result) shouldBe 401
    }

    "return unauthorized when authority record does not contain a NINO" in new NoNino {

      val result = await(controller.getSummary(nino, currentYear)(emptyRequest))

      status(result) shouldBe 401
      contentAsJson(result) shouldBe noNinoOnAccont
    }

    "return unauthorized when authority record has a low CL" in new AuthWithLowConfidence {

      val result = await(controller.getSummary(nino, currentYear)(emptyRequest))

      status(result) shouldBe 401
      contentAsJson(result) shouldBe lowConfidence
    }
  }
}