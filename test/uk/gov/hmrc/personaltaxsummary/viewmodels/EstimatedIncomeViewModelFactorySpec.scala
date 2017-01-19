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
import uk.gov.hmrc.personaltaxsummary.viewmodelfactories.EstimatedIncomeViewModelFactory
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}
import uk.gov.hmrc.play.views.helpers.MoneyPounds
import MessageMatchers._

class EstimatedIncomeViewModelFactorySpec extends UnitSpec with WithFakeApplication with StubApplicationConfiguration with TaiTestData {

  "EstimatedIncomeViewModelFactory createObject" should {
    "create an EstimatedIncomeViewModel instance" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CZ629113A"), nonCodedTaxSummary)

      result shouldBe a[EstimatedIncomeViewModel]
    }

    "have the correct estimated income tax, income and tax free amount" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CZ629113A"), nonCodedTaxSummary)

      result.incomeTaxEstimate shouldBe 14514.6
      result.incomeEstimate shouldBe 62219
      result.taxFreeEstimate shouldBe 10000
    }

    "have a potential underpayment" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CZ629113A"), potentialUnderpaymentTaxSummary)

      result.potentialUnderpayment shouldBe true
    }

    "have outstanding debt" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CZ629113A"), outstandingDebtTaxSummary)

      result.additionalTaxTable should containWrappedMessage("tai.taxCalc.OutstandingDebt.title", MoneyPounds(200, 2).quantity)
    }

    "have child benefit" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CZ629113A"), everythingTaxSummary)

      result.additionalTaxTable should containWrappedMessage("tai.taxCalc.childBenefit.title", MoneyPounds(1500, 2).quantity)
    }

    "return a zero income tax estimate message as reductions are greater than the income tax due" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CN499213B"), reductionsGreaterThanIncomeTaxLiabilityTaxSummary)

      result.incomeTaxEstimate shouldBe 0
      result.incomeTaxReducedToZeroMessage.get should beMessage("tai.estimatedIncome.reductionsTax.incomeTaxReducedToZeroMessage")
    }

    "return a zero income tax estimate message as reductions are equal to the income tax due" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CN499213B"), reductionsEqualToIncomeTaxLiabilityTaxSummary)

      result.incomeTaxEstimate shouldBe 0
      result.incomeTaxReducedToZeroMessage.get should beMessage("tai.estimatedIncome.reductionsTax.incomeTaxReducedToZeroMessage")
    }

    "not return a zero income tax estimate message as reductions are less than the income tax due" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CN499213B"), reductionsLessThanIncomeTaxLiabilityTaxSummary)

      result.incomeTaxEstimate shouldBe 1
      result.incomeTaxReducedToZeroMessage.isDefined shouldBe false
    }
  }
}