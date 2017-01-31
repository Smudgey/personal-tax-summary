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

package uk.gov.hmrc.personaltaxsummary.viewmodels

import data.TaiTestData
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personaltaxsummary.config.StubApplicationConfiguration
import uk.gov.hmrc.personaltaxsummary.domain.TaxSummaryContainer
import uk.gov.hmrc.personaltaxsummary.viewmodelfactories.TaxSummaryContainerFactory
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

class TaxSummaryContainerFactorySpec extends UnitSpec with WithFakeApplication with StubApplicationConfiguration with TaiTestData {

  val nino = Nino("KM569110B")

  "TaxSummaryContainerFactory createObject" should {
    "populate the base view model given a NINO and tax summary" in {
      val result: TaxSummaryContainer = TaxSummaryContainerFactory.createObject(nino, currentYearTaxSummary)

      result.baseViewModel.estimatedIncomeTax shouldBe 1361.4
    }

    "populate estimated income wrapper" in {
      val result: TaxSummaryContainer = TaxSummaryContainerFactory.createObject(nino, nonCodedTaxSummary)

      result.estimatedIncomeWrapper.map(_.estimatedIncome.incomeEstimate) shouldBe Some(62219)
    }

    "populated taxable income" in {
      val result: TaxSummaryContainer = TaxSummaryContainerFactory.createObject(nino, currentYearTaxSummary)

      result.taxableIncome.map(_.income) shouldBe Some(17467)
    }

    "populate gate keeper details and mask estimated and taxable incomes given a user who is gatekeepered" in {
      val result: TaxSummaryContainer = TaxSummaryContainerFactory.createObject(nino, gateKeeperUserTaxSummary)

      result.gatekeeper.isDefined shouldBe true
      result.gatekeeper.map(_.totalLiability.totalTax) shouldBe Some(0)
      result.estimatedIncomeWrapper.map(_.estimatedIncome.incomeEstimate) shouldBe None
      result.taxableIncome.map(_.income) shouldBe None
    }
  }

  "TaxSummaryContainerFactory isGateKeepered" should {
    "should return true given a nino that is gatekeepered" in {
      val result: Boolean = TaxSummaryContainerFactory.isGateKeepered(gateKeeperUserTaxSummary)

      result shouldBe true
    }

    "should return false given a nino that is not gatekeepered" in {
      val result: Boolean = TaxSummaryContainerFactory.isGateKeepered(potentialUnderpaymentTaxSummary)

      result shouldBe false
    }
  }

  "TaxSummaryContainerFactory getPotentialUnderpayment" should {
    "return potential underpayments given a nino with underpayment" in {
      val result: Option[BigDecimal] = TaxSummaryContainerFactory.getPotentialUnderpayment(potentialUnderpaymentTaxSummary)

      result shouldBe Some(4123.29)
    }

    "not return potential underpayments given a nino with no underpayments" in {
      val result: Option[BigDecimal] = TaxSummaryContainerFactory.getPotentialUnderpayment(currentYearTaxSummary)

      result shouldBe None
    }
  }
}
