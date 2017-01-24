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

import org.scalatest.concurrent.ScalaFutures
import play.api.mvc.Result
import play.api.test.FakeApplication
import uk.gov.hmrc.personaltaxsummary.config.StubApplicationConfiguration
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

class PersonalTaxSummaryControllerSpec  extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration {

  override lazy val fakeApplication = FakeApplication(additionalConfiguration = config)

  "getSummary Live" should {

    "return a summary" in new Success {

      val result = await(controller.getSummary(nino, currentYear)(emptyRequest))

      status(result) shouldBe 200

      // TODO: extend test
    }

    // TODO: add tests
//    "return 401 when the nino in the request does not match the authority nino"
//    "return the summary successfully when journeyId is supplied"
//    "return 404 when summary returned is None"
//    "return the gateKeeper summary successfully"
//    "return unauthorized when authority record does not contain a NINO"
//    "return unauthorized when authority record has a low CL"
//    "return status code 406 when the headers are invalid"
  }
}
