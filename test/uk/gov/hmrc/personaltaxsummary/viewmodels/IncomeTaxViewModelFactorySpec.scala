/*
 * Copyright 2018 HM Revenue & Customs
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
import uk.gov.hmrc.model.{Employments, TaxCodeDetails, TaxSummaryDetails}
import uk.gov.hmrc.personaltaxsummary.config.StubApplicationConfiguration
import uk.gov.hmrc.personaltaxsummary.viewmodelfactories.IncomeTaxViewModelFactory
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

class IncomeTaxViewModelFactorySpec extends UnitSpec with WithFakeApplication with StubApplicationConfiguration with TaiTestData {

  "IncomeTaxViewModelFactory createObject" should {
    "create an IncomeTaxViewModel instance" in {
      val result = IncomeTaxViewModelFactory.createObject(Nino("CZ629113A"), currentYearTaxSummary)

      result shouldBe a[IncomeTaxViewModel]
    }

    "create an income tax view model" in {
      val expectedTamc = false
      val expectedChanges = false
      val expectedIncomeTax = 1361.4

      val result = IncomeTaxViewModelFactory.createObject(Nino("CZ629113A"), currentYearTaxSummary)

      result.estimatedIncomeTax shouldBe expectedIncomeTax
      result.hasChanges shouldBe expectedChanges
      result.hasTamc shouldBe expectedTamc
    }

    "create an income tax view model with Tax codes List, taxable income" in {
      val expectedTaxCodes = List("104Y", "500T", "BR")
      val expectedTaxableIncome = 17467

      val result = IncomeTaxViewModelFactory.createObject(Nino("CZ629113A"), currentYearTaxSummary)

      result.taxCodesList shouldBe expectedTaxCodes
      result.taxableIncome shouldBe expectedTaxableIncome
    }

    "create an income tax view model with Personal Allowance, tax free income" in {
      val expectedPersonalAllowance = 10660

      val result = IncomeTaxViewModelFactory.createObject(Nino("CZ629113A"), currentYearTaxSummary)

      result.personalAllowance shouldBe expectedPersonalAllowance
      result.taxFree shouldBe expectedPersonalAllowance
    }

    "create an income tax view model for a simple tax user given a person with a single tax code of 1100L" in {
      val taxSummaryDetailsOneTaxCode = TaxSummaryDetails(
        nino = "CZ629113A",
        version = 1,
        taxCodeDetails = Some(TaxCodeDetails(
          employment = Some(List(
            Employments(
              taxCode = Some("1100L")
            )
          )),
          taxCode = None,
          deductions = None,
          allowances = None
        ))
      )

      val result = IncomeTaxViewModelFactory.createObject(Nino("CZ629113A"), taxSummaryDetailsOneTaxCode)

      result.simpleTaxUser shouldBe true
    }

    "create an income tax view model for a non-simple tax user given a person with more than one tax code of 1100L" in {
      val taxSummaryDetailsMultipleTaxCodes = TaxSummaryDetails(
        nino = "CZ629113A",
        version = 1,
        taxCodeDetails = Some(TaxCodeDetails(
          employment = Some(List(
            Employments(
              taxCode = Some("1100L")
            ),
            Employments(
              taxCode = Some("1100L")
            )
          )),
          taxCode = None,
          deductions = None,
          allowances = None
        ))
      )

      val result = IncomeTaxViewModelFactory.createObject(Nino("CZ629113A"), taxSummaryDetailsMultipleTaxCodes)

      result.simpleTaxUser shouldBe false
    }

    "create an income tax view model for a non-simple tax user given a person with a single tax code that is not 1100L" in {
      val taxSummaryDetailsOneOtherTaxCode = TaxSummaryDetails(
        nino = "CZ629113A",
        version = 1,
        taxCodeDetails = Some(TaxCodeDetails(
          employment = Some(List(
            Employments(
              taxCode = Some("500L")
            )
          )),
          taxCode = None,
          deductions = None,
          allowances = None
        ))
      )

      val result = IncomeTaxViewModelFactory.createObject(Nino("CZ629113A"), taxSummaryDetailsOneOtherTaxCode)

      result.simpleTaxUser shouldBe false
    }

    "create an income tax view model for a non-simple tax user given a person with more than one tax code, none of which are 1100L" in {
      val taxSummaryDetailsMultiOtherTaxCodes = TaxSummaryDetails(
        nino = "CZ629113A",
        version = 1,
        taxCodeDetails = Some(TaxCodeDetails(
          employment = Some(List(
            Employments(
              taxCode = Some("D0")
            ),
            Employments(
              taxCode = Some("D1")
            )
          )),
          taxCode = None,
          deductions = None,
          allowances = None
        ))
      )

      val result = IncomeTaxViewModelFactory.createObject(Nino("CZ629113A"), taxSummaryDetailsMultiOtherTaxCodes)

      result.simpleTaxUser shouldBe false
    }

    "create an income tax view model for a non-simple tax user given a person with no tax codes" in {
      val taxSummaryDetailsNoTaxCodes = TaxSummaryDetails(
        nino = "CZ629113A",
        version = 1,
        taxCodeDetails = Some(TaxCodeDetails(
          employment = Some(List()),
          taxCode = None,
          deductions = None,
          allowances = None
        ))
      )

      val result = IncomeTaxViewModelFactory.createObject(Nino("CZ629113A"), taxSummaryDetailsNoTaxCodes)

      result.simpleTaxUser shouldBe false
    }
  }
}