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
import play.api.i18n.Messages
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.model.{IabdSummary, NoneTaxCodeIncomes, TaxComponent}
import uk.gov.hmrc.personaltaxsummary.config.StubApplicationConfiguration
import uk.gov.hmrc.personaltaxsummary.viewmodelfactories.{YourTaxableIncomeHelper, YourTaxableIncomeViewModelFactory}
import uk.gov.hmrc.personaltaxsummary.viewmodels.WrappedDataMatchers._
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}
import uk.gov.hmrc.play.views.helpers.MoneyPounds
import play.api.Play.current
import play.api.i18n.Messages.Implicits._

class YourTaxableIncomeViewModelFactorySpec extends UnitSpec with WithFakeApplication with StubApplicationConfiguration with TaiTestData {
  "IncomeTaxViewModelFactory createObject" should {
    "create a YourTaxableIncomeViewModel instance" in {
      val result = YourTaxableIncomeViewModelFactory.createObject(Nino("CZ629113A"), currentYearTaxSummary)

      result shouldBe a[YourTaxableIncomeViewModel]
    }

    "create the your taxable income view page model" in {
      val expectedTaxCodesList = List("104Y", "500T", "BR")
      val expectedTaxFreeAmount = 10660
      val expectedIncome = 17467

      val result = YourTaxableIncomeViewModelFactory.createObject(Nino("CZ629113A"), currentYearTaxSummary)

      result.taxCodeList shouldBe expectedTaxCodesList
      result.taxFreeAmount shouldBe expectedTaxFreeAmount
      result.income shouldBe expectedIncome
    }

    "create the your taxable income view page model with benefits data" in {
      val result = YourTaxableIncomeViewModelFactory.createObject(Nino("CZ629113A"), bankInterestTaxSummary)

      result.benefitsData.size shouldBe 4
      result.benefitsData should containWrappedBenefitsData(preformat("tai.iabdSummary.employmentBenefit.type-44", "PAYESCHEMEOPERATORNAME53446"), MoneyPounds(800, 0).quantity, "tai.iabdSummary.description-44", "", Some(1), Some(44))
      result.benefitsTotal shouldBe 1250

      result.taxableBenefitsData.size shouldBe 5
      result.taxableBenefitsData.contains((Messages("tai.income.statePension.title"), MoneyPounds(100, 0).quantity, Messages("tai.iabdSummary.description-state-pension"))) shouldBe true
      result.taxableBenefitsTotal shouldBe 5400
    }

    "create the your taxable income view page model with investment and other income data" in {
      val result = YourTaxableIncomeViewModelFactory.createObject(Nino("CZ629113A"), bankInterestTaxSummary)

      result.investmentIncomeData.contains((Messages("tai.iabdSummary.type-82"), MoneyPounds(3000, 0).quantity, Messages("tai.iabdSummary.description-82"))) shouldBe true
      result.investmentIncomeData.contains((Messages("tai.iabdSummary.type-75"), MoneyPounds(5000, 0).quantity, Messages("tai.iabdSummary.description-75"))) shouldBe true
      result.investmentIncomeTotal shouldBe 28000

      result.otherIncomeData.contains((Messages("tai.iabdSummary.type-25"), MoneyPounds(800, 0).quantity, Messages("tai.iabdSummary.description-25"))) shouldBe true
      result.otherIncomeTotal shouldBe 3300
    }

    "create the your taxable income view page model with pension data" in {
      val result = YourTaxableIncomeViewModelFactory.createObject(Nino("CZ629113A"), bankInterestTaxSummary)

      result.employmentPension.hasEmployment shouldBe true
      result.employmentPension.totalEmploymentPensionAmt shouldBe 5000
    }

    "create the your taxable income view page model with correct population of occupational pension data" in {
      val result = YourTaxableIncomeViewModelFactory.createObject(Nino("CZ629113A"), allEditableIncomesAndNonEditableCeaseTaxSummary)

      result.employmentPension.hasEmployment shouldBe true
      result.employmentPension.isOccupationalPension shouldBe true
    }
  }

  "YourTaxableIncomeHelper" should {
    "create an Investment Income table given Dividends, Bank Interest and Untaxed Interest are available" in {
      val divIabd = IabdSummary(76, "UK Dividend", 2000, Some(1), Some("Sainsburys"))
      val divIabdList = List(divIabd)

      val bankIntIabd = IabdSummary(75, "Bank Interest", 50, Some(1), Some("Sainsburys"))
      val bankIntIabdList = List(bankIntIabd)

      val unTaxedBankIntIabd = IabdSummary(82, "Untaxed Interest", 100, Some(1), Some("Sainsburys"))
      val unTaxedBankIntIabdList = List(unTaxedBankIntIabd)

      val ukDivTaxComponent = TaxComponent(2000, 0, "UK Dividend", divIabdList)
      val bsTaxComponent = TaxComponent(50, 0, "Bank Interest", bankIntIabdList)
      val unTaxedTaxComponent = TaxComponent(100, 0, "UnTaxed Interest", unTaxedBankIntIabdList)

      val noneTaxableIncomes = NoneTaxCodeIncomes(statePension = None,
        statePensionLumpSum = None, otherPensions = None, otherIncome = None,
        taxableStateBenefit = None, dividends = Some(ukDivTaxComponent),
        untaxedInterest = Some(unTaxedTaxComponent), bankBsInterest = Some(bsTaxComponent),
        totalIncome = 2150)

      val result = YourTaxableIncomeHelper.createInvestmentIncomeTable(Some(noneTaxableIncomes))

      result._1.size shouldBe 3
      result._2 shouldBe 2150
    }

    "create a Taxable Benefits table given taxable benefits are available" in {
      val iabdEmpList = List(IabdSummary(83, "Incapacity Benefit", 100, Some(1), Some("Sainsburys")),
        IabdSummary(84, "Job Seekers Allowance", 100, Some(1), Some("Sainsburys")))

      val taxableStateBenefit = TaxComponent(200, 0, "Taxable State Benefits", iabdEmpList)

      val noneTaxableIncomes = NoneTaxCodeIncomes(statePension = Some(BigDecimal(50)),
        statePensionLumpSum = Some(BigDecimal(120)), otherPensions = None, otherIncome = None,
        taxableStateBenefit = Some(taxableStateBenefit), dividends = None,
        untaxedInterest = None, bankBsInterest = None,
        totalIncome = 370)

      val result = YourTaxableIncomeHelper.createTaxableBenefitTable(Some(noneTaxableIncomes), None)
      result._1.size shouldBe 4
      result._2 shouldBe 370
    }

    "create an empty Taxable Benefits table given no taxable benefits are available" in {
      val result = YourTaxableIncomeHelper.createTaxableBenefitTable(None, None)

      result._1.size shouldBe 0
      result._2 shouldBe 0
    }

    "create an Other Income table given Other Income is available" in {
      val profitIabd = IabdSummary(72, "Profit", 2000, Some(1), Some("Sainsburys"))
      val otherIncomeIabd = IabdSummary(19, "Other Income", 2000, Some(1), Some("Sainsburys"))
      val otherIncomeIabdList = List(profitIabd, otherIncomeIabd)

      val otherPensionIabdList = List(IabdSummary(71, "Other Pension", 100, Some(1), Some("Sainsburys")), IabdSummary(70, "Other Pension Desc", 200, Some(1), Some("Sainsburys")))
      val otherPensionTaxComponent = TaxComponent(300, 0, "Other Pension", otherPensionIabdList)

      val otherIncomeTaxComponent = TaxComponent(4000, 0, "Other Income", otherIncomeIabdList)

      val noneTaxableIncomes = NoneTaxCodeIncomes(statePension = None,
        statePensionLumpSum = None, otherPensions = Some(otherPensionTaxComponent), otherIncome = None,
        taxableStateBenefit = None, dividends = None,
        untaxedInterest = None, bankBsInterest = None,
        totalIncome = 2000)

      val result = YourTaxableIncomeHelper.createOtherIncomeTable(Some(noneTaxableIncomes), otherIncomeTaxComponent.iabdSummaries)

      result._1.size shouldBe 4
      result._2 shouldBe 4300
    }

    "create an Employer Benefits table given employment benefits are available" in {
      val expectedUrl = "/link/to/med/benefits/service"
      val expectedBenefit = "Medical insurance"
      val expectedEmployer = "Sainsburys"

      val links = Map(
        "companyCarServiceUrl" -> "/link/to/company/car/service",
        "medBenefitServiceUrl" -> expectedUrl
      )

      val empBenefitsIabd = List(IabdSummary(53, "Travel and Subsistence", 100, Some(1), Some(expectedEmployer)),
        IabdSummary(29, "Car fuel benefit", 100, Some(1), Some(expectedEmployer)),
        IabdSummary(30, expectedBenefit, 100, Some(1), Some(expectedEmployer)))

      val taxComponent = TaxComponent(300, 0, "Employer Benefits", empBenefitsIabd)

      val result: (List[(String, String, String, String, Option[Int], Option[Int])], BigDecimal) = YourTaxableIncomeHelper.createBenefitsTable(taxComponent, links)

      result._1.size shouldBe 3
      result._2 shouldBe 300
      result._1.last._1 shouldBe s"$expectedBenefit for $expectedEmployer"
      result._1.last._4 shouldBe expectedUrl
    }

    "create an Employer Benefits table given accommodation benefit is available" in {

      val empBenefitsIabd = List(IabdSummary(38, "", 100, None, Some("TESCO")))

      val taxComponent = TaxComponent(100, 0, "Employer Benefits", empBenefitsIabd)

      val (benefitDetails, amount) = YourTaxableIncomeHelper.createBenefitsTable(taxComponent, Map.empty)

      benefitDetails.size shouldBe 1
      amount shouldBe 100
      benefitDetails.head._1 shouldBe  Messages("tai.iabdSummary.employmentBenefit.type-38", "TESCO")
    }

    "create an Employer Benefits table given Assets transferred benefit is available" in {

      val empBenefitsIabd = List(IabdSummary(40, "", 100, None, Some("TESCO")))

      val taxComponent = TaxComponent(100, 0, "Employer Benefits", empBenefitsIabd)

      val (benefitDetails, amount) = YourTaxableIncomeHelper.createBenefitsTable(taxComponent, Map.empty)

      benefitDetails.size shouldBe 1
      amount shouldBe 100
      benefitDetails.head._1 shouldBe Messages("tai.iabdSummary.employmentBenefit.type-40", "TESCO")
    }

    "create an Employer Benefits table given remuneration benefit is available" in {

      val empBenefitsIabd = List(IabdSummary(52, "", 100, None, Some("TESCO")))

      val taxComponent = TaxComponent(100, 0, "Employer Benefits", empBenefitsIabd)

      val (benefitDetails, amount) = YourTaxableIncomeHelper.createBenefitsTable(taxComponent, Map.empty)

      benefitDetails.size shouldBe 1
      amount shouldBe 100
      benefitDetails.head._1 shouldBe Messages("tai.iabdSummary.employmentBenefit.type-52", "TESCO")
    }

    "create an Employer Benefits table given no employment benefits" in {
      val taxComponent = TaxComponent(0, 0, "Employer Benefits", List())

      val result = YourTaxableIncomeHelper.createBenefitsTable(taxComponent, Map.empty)

      result._1.size shouldBe 0
      result._2 shouldBe 0
    }
  }
}