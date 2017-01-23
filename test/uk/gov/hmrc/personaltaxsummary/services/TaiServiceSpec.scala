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

import uk.gov.hmrc.model.GateKeeperRule
import uk.gov.hmrc.personaltaxsummary.domain.TaxSummaryContainer
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

import scala.concurrent.ExecutionContext.Implicits.global

class TaiServiceSpec extends UnitSpec with WithFakeApplication with Setup {

  "TaiService getSummary" should {
    "return a tax summary container given a valid NINO and year" in {
      val result: Option[TaxSummaryContainer] = await(TaiServiceTest.getSummary(nino, currentYear))

      result.isDefined shouldBe true
    }

    "populate the base view model" in {
      val result: TaxSummaryContainer = await(TaiServiceTest.getSummary(nino, currentYear)).getOrElse(fail("missing TaxSummaryContainer"))

      result.baseViewModel.estimatedIncomeTax shouldBe 1361.4
    }

    "populate estimated income wrapper" in {
      val result: TaxSummaryContainer = await(TaiServiceTest.getSummary(nonCoded, currentYear)).getOrElse(fail("missing TaxSummaryContainer"))

      result.estimatedIncomeWrapper.map(_.estimatedIncome.incomeEstimate) shouldBe Some(62219)
    }

    "populated taxable income" in {
      val result: TaxSummaryContainer = await(TaiServiceTest.getSummary(nino, currentYear)).getOrElse(fail("missing TaxSummaryContainer"))

      result.taxableIncome.map(_.income) shouldBe Some(17467)
    }

    "populate gate keeper details and mask estimated and taxable incomes given a user who is gatekeepered" in {
      val result: TaxSummaryContainer = await(TaiServiceTest.getSummary(gateKeepered, currentYear)).getOrElse(fail("missing TaxSummaryContainer"))

      result.gatekeeper.isDefined shouldBe true
      result.gatekeeper.map(_.gateKeepered) shouldBe Some(true)
      result.estimatedIncomeWrapper.map(_.estimatedIncome.incomeEstimate) shouldBe None
      result.taxableIncome.map(_.income) shouldBe None
    }
  }

  "TaiService isGateKeepered" should {
    "should return true given a nino that is gatekeepered" in {
      val result: Boolean = TaiServiceTest.isGateKeepered(gateKeeperUserTaxSummary)

      result shouldBe true
    }

    "should return false given a nino that is not gatekeepered" in {
      val result: Boolean = TaiServiceTest.isGateKeepered(potentialUnderpaymentTaxSummary)

      result shouldBe false
    }
  }

  "TaiService getPotentialUnderpayment" should {
    "return potential underpayments given a nino with underpayment" in {
      val result: Option[BigDecimal] = TaiServiceTest.getPotentialUnderpayment(potentialUnderpaymentTaxSummary)

      result shouldBe Some(4123.29)
    }

    "not return potential underpayments given a nino with no underpayments" in {
      val result: Option[BigDecimal] = TaiServiceTest.getPotentialUnderpayment(currentYearTaxSummary)

      result shouldBe None
    }
  }
}