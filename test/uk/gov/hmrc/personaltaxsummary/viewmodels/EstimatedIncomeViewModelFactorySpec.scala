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
import play.api.Play.current
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._
import uk.gov.hmrc.model.{TaxSummaryDetails, nps2}
import uk.gov.hmrc.model.nps2.{TaxAccount, TaxBand, TaxDetail, TaxObject}
import uk.gov.hmrc.model.tai.{AnnualAccount, TaxYear}

class EstimatedIncomeViewModelFactorySpec extends UnitSpec with WithFakeApplication with StubApplicationConfiguration with TaiTestData {

  "EstimatedIncomeViewModelFactory createObject" should {

    val annualAccounts = List(AnnualAccount(TaxYear(2016), Some(TaxAccount(None,None,1564.45,
      Map(TaxObject.Type.NonSavings -> TaxDetail(Some(1111.11),Some(9969),None,
        Some(Seq(nps2.TaxBand(Some("pa"),None,2290,0,None,None,0),
          nps2.TaxBand(Some("B"),None,9969,1993.80,Some(0),Some(33125),20.00)))))))))

    "create an EstimatedIncomeViewModel instance" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CZ629113A"), nonCodedTaxSummary.copy(accounts = annualAccounts))

      result shouldBe a[EstimatedIncomeViewModel]
    }

    "have the correct estimated income tax, income and tax free amount" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CZ629113A"), nonCodedTaxSummary.copy(accounts = annualAccounts))

      result.incomeTaxEstimate shouldBe 14514.6
      result.incomeEstimate shouldBe 62219
      result.taxFreeEstimate shouldBe 10000
    }

    "have a potential underpayment" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CZ629113A"), potentialUnderpaymentTaxSummary.copy(accounts = annualAccounts))

      result.potentialUnderpayment shouldBe true
    }

    "have outstanding debt" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CZ629113A"), outstandingDebtTaxSummary.copy(accounts = annualAccounts))

      result.additionalTaxTable shouldBe List((Messages("tai.taxCalc.OutstandingDebt.title"), MoneyPounds(200, 2).quantity))
    }

    "have child benefit" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CZ629113A"), everythingTaxSummary.copy(accounts = annualAccounts))

      result.additionalTaxTable.contains((Messages("tai.taxCalc.childBenefit.title"), MoneyPounds(1500, 2).quantity)) shouldBe true
    }

    "not return a zero income tax estimate message as reductions are less than the income tax due" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CN499213B"), reductionsLessThanIncomeTaxLiabilityTaxSummary)

      result.incomeTaxEstimate shouldBe 1
      result.incomeTaxReducedToZeroMessage.isDefined shouldBe false
    }
  }

  "merge Tax bands" should {

    "return one tax band for 20% rate and one tax band for 40% rate" in {
      val taxBand = List(TaxBand(None, None, income = 1000, tax = 20, lowerBand = None, upperBand = Some(4000), rate = 20),
        TaxBand(None, None, income = 2500, tax = 40, lowerBand = None, upperBand = Some(5000), rate = 20),
        TaxBand(None, None, income = 1000, tax = 20, lowerBand = None, upperBand = Some(4000), rate = 40),
        TaxBand(None, None, income = 2000, tax = 20, lowerBand = None, upperBand = Some(4000), rate = 40))

      val dataF = EstimatedIncomeViewModelFactory.mergedBands(taxBand)
      dataF shouldBe List(Band("Band2", 87.5, "20", 3500, 60), Band("Band2", 75, "40", 3000, 40))
    }
  }

  "individual Tax bands" should {

    "return two tax bands for 0% rate" in {
      val taxBand = List(TaxBand(None, None, income = 1000, tax = 0, lowerBand = None, upperBand = Some(5000), rate = 0),
        TaxBand(None, None, income = 2000, tax = 0, lowerBand = None, upperBand = Some(5000), rate = 0))

      val dataF = EstimatedIncomeViewModelFactory.individualBands(taxBand)
      dataF shouldBe List(Band("TaxFree", 20, "0", 1000, 0), Band("TaxFree", 40, "0", 2000, 0))
    }
  }

  "bandedGraph" should {
    "have all the list of bands to display" in {
      val taxBand = List(TaxBand(None, None, income = 1000, tax = 20, lowerBand = None, upperBand = None, rate = 20),
        TaxBand(None, None, income = 1000, tax = 20, lowerBand = None, upperBand = None, rate = 20),
        TaxBand(None, None, income = 1000, tax = 0, lowerBand = None, upperBand = None, rate = 0),
        TaxBand(None, None, income = 1000, tax = 0, lowerBand = None, upperBand = None, rate = 0),
        TaxBand(None, None, income = 1000, tax = 40, lowerBand = None, upperBand = None, rate = 40),
        TaxBand(None, None, income = 1000, tax = 40, lowerBand = None, upperBand = Some(5000), rate = 40))

      val bands = List(Band("TaxFree",20,"0",1000,0), Band("TaxFree",20,"0",1000,0),
        Band("Band2",40,"20",2000,40), Band("Band2",40,"40",2000,80))

      val taxObjects: Map[TaxObject.Type.Value, TaxDetail] = Map({TaxObject.Type.BankInterest -> TaxDetail(taxBands = Some(taxBand))})
      val taxAccount = TaxAccount(None, None, tax = 1000,
        taxObjects = taxObjects)
      val accounts = List(AnnualAccount(TaxYear(2016),Some(taxAccount)))
      val testTaxSummary = TaxSummaryDetails(nino = "", version = 0, accounts = accounts)

      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(testTaxSummary)
      dataF shouldBe BandedGraph("taxGraph", bands, 0,5000,6000,120,120)
    }
  }
}