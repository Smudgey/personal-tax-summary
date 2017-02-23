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
import play.api.libs.json.Json
import play.api.mvc.Result
import play.api.test.FakeApplication
import play.api.test.Helpers.contentAsJson
import uk.gov.hmrc.personaltaxsummary.config.StubApplicationConfiguration
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

class PersonalTaxSummaryDomainControllerContractSpec  extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration {

  override lazy val fakeApplication = FakeApplication(additionalConfiguration = config)

  "buildEstimatedIncome Live" should {
    "return an EstimatedIncomeViewModel" in new Setup {

      val result: Result = await(domainController.buildEstimatedIncome(nino)(taxSummaryDetailsRequest))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(estimatedIncomeViewModel)
    }

    "return BadRequest given an invalid request" in new Setup  {
      val result: Result = await(domainController.buildEstimatedIncome(nino, Some(journeyId))(badRequest))

      status(result) shouldBe 400
    }
  }

  "buildYourTaxableIncome Live" should {
    "return a YourTaxableIncomeViewModel" in new Setup {

      val result: Result = await(domainController.buildYourTaxableIncome(nino, Some(journeyId))(taxSummaryDetailsRequest))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(yourTaxableIncomeViewModel)
    }

    "return BadRequest given an invalid request" in new Setup {
      val result: Result = await(domainController.buildYourTaxableIncome(nino)(badRequest))

      status(result) shouldBe 400
    }
  }
}
