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
import play.api.Play.current
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.model.nps2.{TaxAccount, TaxBand, TaxDetail, TaxObject}
import uk.gov.hmrc.model.tai.{AnnualAccount, TaxYear}
import uk.gov.hmrc.model.{TaxSummaryDetails, nps2}
import uk.gov.hmrc.personaltaxsummary.config.StubApplicationConfiguration
import uk.gov.hmrc.personaltaxsummary.viewmodelfactories.EstimatedIncomeViewModelFactory
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}
import uk.gov.hmrc.play.views.helpers.MoneyPounds

class EstimatedIncomeViewModelFactorySpec extends UnitSpec with WithFakeApplication with StubApplicationConfiguration with TaiTestData {

  "EstimatedIncomeViewModelFactory createObject" should {

    val annualAccounts = List(AnnualAccount(TaxYear(2016), Some(TaxAccount(None, None, 1564.45,
      Map(TaxObject.Type.NonSavings -> TaxDetail(Some(1111.11), Some(9969), None,
        Some(Seq(nps2.TaxBand(Some("pa"), None, 2290, 0, None, None, 0),
          nps2.TaxBand(Some("B"), None, 9969, 1993.80, Some(0), Some(33125), 20.00)))))))))

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

    "return only one merged tax band for other than zero% rate band" in {
      val taxBand = List(TaxBand(None, None, income = 1000, tax = 20, lowerBand = None, upperBand = Some(4000), rate = 20),
        TaxBand(None, None, income = 2500, tax = 40, lowerBand = None, upperBand = Some(5000), rate = 20),
        TaxBand(None, None, income = 1000, tax = 20, lowerBand = None, upperBand = Some(4000), rate = 40),
        TaxBand(None, None, income = 2000, tax = 20, lowerBand = None, upperBand = Some(4000), rate = 40))

      val dataF = EstimatedIncomeViewModelFactory.mergedBands(taxBand)
      dataF.get shouldBe Band("Band", 100, "Check in more detail", 6500, 100, "TaxedIncome")
    }
  }

  "individual Tax bands" should {

    "return two tax bands for 0% rate" in {
      val taxBand = List(TaxBand(None, None, income = 1000, tax = 0, lowerBand = None, upperBand = Some(5000), rate = 0),
        TaxBand(None, None, income = 2000, tax = 0, lowerBand = None, upperBand = Some(5000), rate = 0))

      val dataF = EstimatedIncomeViewModelFactory.individualBands(taxBand)
      dataF shouldBe List(Band("TaxFree", 20, "0%", 1000, 0, "NA"), Band("TaxFree", 40, "0%", 2000, 0, "NA"))
    }
  }

  "bandedGraph" should {
    "have two bands(0&20) to display in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 3200, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("B"), None, income = 16000, tax = 5000, lowerBand = Some(11000), upperBand = Some(32000), rate = 20)
      )

      val bands = List(
        Band("TaxFree", 10, "0%", 3200, 0, "pa"),
        Band("Band", 50, "20%", 16000, 5000, "B")
      )

      val taxObjects: Map[TaxObject.Type.Value, TaxDetail] = Map({
        TaxObject.Type.BankInterest -> TaxDetail(taxBands = Some(taxBand))
      })
      val taxAccount = TaxAccount(None, None, tax = 1000,
        taxObjects = taxObjects)
      val accounts = List(AnnualAccount(TaxYear(2016), Some(taxAccount)))
      val testTaxSummary = TaxSummaryDetails(nino = "", version = 0, accounts = accounts)

      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(testTaxSummary)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 32000, 19200, 10, 3200, 60, 5000)
    }

    "have two bands(0 & Taxed Income) to display in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 3000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("B"), None, income = 15000, tax = 3000, lowerBand = Some(11000), upperBand = Some(32000), rate = 20),
        TaxBand(Some("D0"), None, income = 30000, tax = 12000, lowerBand = Some(32000), upperBand = Some(150000), rate = 40)
      )

      val bands = List(
        Band("TaxFree", 2, "0%", 3000, 0, "pa"),
        Band("Band", 30, "Check in more detail", 45000, 15000, "TaxedIncome")
      )

      val taxObjects: Map[TaxObject.Type.Value, TaxDetail] = Map({
        TaxObject.Type.BankInterest -> TaxDetail(taxBands = Some(taxBand))
      })
      val taxAccount = TaxAccount(None, None, tax = 1000,
        taxObjects = taxObjects)
      val accounts = List(AnnualAccount(TaxYear(2016), Some(taxAccount)))
      val testTaxSummary = TaxSummaryDetails(nino = "", version = 0, accounts = accounts)

      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(testTaxSummary)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 150000, 48000, 2, 3000, 32, 15000)
    }

    "have two bands(0 & Taxed Income) for multiple other band to display in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 5000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("B"), None, income = 15000, tax = 3000, lowerBand = Some(11000), upperBand = Some(32000), rate = 20),
        TaxBand(Some("D0"), None, income = 150000, tax = 60000, lowerBand = Some(32000), upperBand = Some(150000), rate = 40),
        TaxBand(Some("D1"), None, income = 30000, tax = 2250, lowerBand = Some(150000), upperBand = Some(0), rate = 45)
      )

      val bands = List(
        Band("TaxFree", 2.5, "0%", 5000, 0, "pa"),
        Band("Band", 97.5, "Check in more detail", 195000, 65250, "TaxedIncome")
      )

      val taxObjects: Map[TaxObject.Type.Value, TaxDetail] = Map({
        TaxObject.Type.BankInterest -> TaxDetail(taxBands = Some(taxBand))
      })
      val taxAccount = TaxAccount(None, None, tax = 1000,
        taxObjects = taxObjects)
      val accounts = List(AnnualAccount(TaxYear(2016), Some(taxAccount)))
      val testTaxSummary = TaxSummaryDetails(nino = "", version = 0, accounts = accounts)

      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(testTaxSummary)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 200000, 200000, 2.5, 5000, 100, 65250)
    }

    "have one band(Taxed Income) for multiple other band to display in graph" in {

      val taxBand = List(
        TaxBand(Some("B"), None, income = 20000, tax = 3000, lowerBand = Some(11000), upperBand = Some(32000), rate = 20),
        TaxBand(Some("D0"), None, income = 150000, tax = 60000, lowerBand = Some(32000), upperBand = Some(150000), rate = 40),
        TaxBand(Some("D1"), None, income = 30000, tax = 2250, lowerBand = Some(150000), upperBand = Some(0), rate = 45)
      )

      val bands = List(
        Band("Band", 100, "Check in more detail", 200000, 65250, "TaxedIncome")
      )

      val taxObjects: Map[TaxObject.Type.Value, TaxDetail] = Map({
        TaxObject.Type.BankInterest -> TaxDetail(taxBands = Some(taxBand))
      })
      val taxAccount = TaxAccount(None, None, tax = 1000,
        taxObjects = taxObjects)
      val accounts = List(AnnualAccount(TaxYear(2016), Some(taxAccount)))
      val testTaxSummary = TaxSummaryDetails(nino = "", version = 0, accounts = accounts)

      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(testTaxSummary)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 200000, 200000, 0, 0, 100, 65250)
    }

    "have two 0 % band and one 20% band in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 11000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("SR"), None, income = 3000, tax = 0, lowerBand = Some(11000), upperBand = Some(14000), rate = 0),
        TaxBand(Some("D0"), None, income = 15000, tax = 3000, lowerBand = Some(14000), upperBand = Some(32000), rate = 20)
      )

      val bands = List(
        Band("TaxFree", 34.375, "0%", 11000, 0, "pa"),
        Band("TaxFree", 9.375, "0%", 3000, 0, "SR"),
        Band("Band", 46.875, "20%", 15000, 3000, "D0")
      )

      val taxObjects: Map[TaxObject.Type.Value, TaxDetail] = Map({
        TaxObject.Type.BankInterest -> TaxDetail(taxBands = Some(taxBand))
      })
      val taxAccount = TaxAccount(None, None, tax = 1000,
        taxObjects = taxObjects)
      val accounts = List(AnnualAccount(TaxYear(2016), Some(taxAccount)))
      val testTaxSummary = TaxSummaryDetails(nino = "", version = 0, accounts = accounts)

      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(testTaxSummary)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 32000, 29000, 43.750, 14000, 90.625, 3000)
    }

    "have two 0 % band and one Taxed Income band in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 10000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("SR"), None, income = 10000, tax = 0, lowerBand = Some(11000), upperBand = Some(14000), rate = 0),
        TaxBand(Some("B"), None, income = 10000, tax = 3000, lowerBand = Some(14000), upperBand = Some(32000), rate = 20),
        TaxBand(Some("B"), None, income = 10000, tax = 3000, lowerBand = Some(14000), upperBand = Some(50000), rate = 20)
      )

      val bands = List(
        Band("TaxFree", 20, "0%", 10000, 0, "pa"),
        Band("TaxFree", 20, "0%", 10000, 0, "SR"),
        Band("Band", 40, "Check in more detail", 20000, 6000, "TaxedIncome")
      )

      val taxObjects: Map[TaxObject.Type.Value, TaxDetail] = Map({
        TaxObject.Type.BankInterest -> TaxDetail(taxBands = Some(taxBand))
      })
      val taxAccount = TaxAccount(None, None, tax = 1000,
        taxObjects = taxObjects)
      val accounts = List(AnnualAccount(TaxYear(2016), Some(taxAccount)))
      val testTaxSummary = TaxSummaryDetails(nino = "", version = 0, accounts = accounts)

      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(testTaxSummary)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 50000, 40000, 40, 20000, 80, 6000)
    }

    "have two 0 % band and one 7.5% band in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 11000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("SR"), None, income = 3000, tax = 0, lowerBand = Some(11000), upperBand = Some(14000), rate = 0),
        TaxBand(Some("SDR"), None, income = 15000, tax = 2000, lowerBand = Some(14000), upperBand = Some(32000), rate = 7.5)
      )

      val bands = List(
        Band("TaxFree", 34.375, "0%", 11000, 0, "pa"),
        Band("TaxFree", 9.375, "0%", 3000, 0, "SR"),
        Band("Band", 46.875, "7.5%", 15000, 2000, "SDR")
      )

      val taxObjects: Map[TaxObject.Type.Value, TaxDetail] = Map({
        TaxObject.Type.BankInterest -> TaxDetail(taxBands = Some(taxBand))
      })
      val taxAccount = TaxAccount(None, None, tax = 1000,
        taxObjects = taxObjects)
      val accounts = List(AnnualAccount(TaxYear(2016), Some(taxAccount)))
      val testTaxSummary = TaxSummaryDetails(nino = "", version = 0, accounts = accounts)

      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(testTaxSummary)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 32000, 29000, 43.750, 14000, 90.625, 2000)
    }

    "have three 0 % band and zero other band in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 11000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("SR"), None, income = 3000, tax = 0, lowerBand = Some(11000), upperBand = Some(14000), rate = 0),
        TaxBand(Some("SDR"), None, income = 15000, tax = 0, lowerBand = Some(14000), upperBand = Some(32000), rate = 0)
      )

      val bands = List(
        Band("TaxFree", 34.375, "0%", 11000, 0, "pa"),
        Band("TaxFree", 9.375, "0%", 3000, 0, "SR"),
        Band("TaxFree", 46.875, "0%", 15000, 0, "SDR")
      )

      val taxObjects: Map[TaxObject.Type.Value, TaxDetail] = Map({
        TaxObject.Type.BankInterest -> TaxDetail(taxBands = Some(taxBand))
      })
      val taxAccount = TaxAccount(None, None, tax = 1000,
        taxObjects = taxObjects)
      val accounts = List(AnnualAccount(TaxYear(2016), Some(taxAccount)))
      val testTaxSummary = TaxSummaryDetails(nino = "", version = 0, accounts = accounts)

      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(testTaxSummary)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 32000, 29000, 90.625, 29000, 90.625, 0)
    }

    "have two 0 % band and one Taxed Income band(7.5 & 20 ) in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 10000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("SR"), None, income = 10000, tax = 0, lowerBand = Some(11000), upperBand = Some(14000), rate = 0),
        TaxBand(Some("SDR"), None, income = 10000, tax = 750, lowerBand = Some(14000), upperBand = Some(32000), rate = 7.5),
        TaxBand(Some("B"), None, income = 10000, tax = 3000, lowerBand = Some(14000), upperBand = Some(50000), rate = 20)
      )

      val bands = List(
        Band("TaxFree", 20, "0%", 10000, 0, "pa"),
        Band("TaxFree", 20, "0%", 10000, 0, "SR"),
        Band("Band", 40, "Check in more detail", 20000, 3750, "TaxedIncome")
      )

      val taxObjects: Map[TaxObject.Type.Value, TaxDetail] = Map({
        TaxObject.Type.BankInterest -> TaxDetail(taxBands = Some(taxBand))
      })
      val taxAccount = TaxAccount(None, None, tax = 1000,
        taxObjects = taxObjects)
      val accounts = List(AnnualAccount(TaxYear(2016), Some(taxAccount)))
      val testTaxSummary = TaxSummaryDetails(nino = "", version = 0, accounts = accounts)

      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(testTaxSummary)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 50000, 40000, 40, 20000, 80, 3750)
    }

    "have two 0 % band and one Taxed Income band(7.5 & 20 & 45) in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 10000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("SR"), None, income = 10000, tax = 0, lowerBand = Some(11000), upperBand = Some(14000), rate = 0),
        TaxBand(Some("B"), None, income = 10000, tax = 750, lowerBand = Some(14000), upperBand = Some(32000), rate = 20),
        TaxBand(Some("D0"), None, income = 10000, tax = 3000, lowerBand = Some(14000), upperBand = Some(100000), rate = 40),
        TaxBand(Some("D1"), None, income = 20000, tax = 3000, lowerBand = Some(100000), upperBand = Some(0), rate = 45)
      )

      val bands = List(
        Band("TaxFree", 10, "0%", 10000, 0, "pa"),
        Band("TaxFree", 10, "0%", 10000, 0, "SR"),
        Band("Band", 40, "Check in more detail", 40000, 6750, "TaxedIncome")
      )

      val taxObjects: Map[TaxObject.Type.Value, TaxDetail] = Map({
        TaxObject.Type.BankInterest -> TaxDetail(taxBands = Some(taxBand))
      })
      val taxAccount = TaxAccount(None, None, tax = 1000,
        taxObjects = taxObjects)
      val accounts = List(AnnualAccount(TaxYear(2016), Some(taxAccount)))
      val testTaxSummary = TaxSummaryDetails(nino = "", version = 0, accounts = accounts)

      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(testTaxSummary)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 100000, 60000, 20, 20000, 60, 6750)
    }

    "have two 0 % band and one Taxed Income band(7.5 & 20 & 45 & 60) in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 10000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("SR"), None, income = 10000, tax = 0, lowerBand = Some(11000), upperBand = Some(14000), rate = 0),
        TaxBand(Some("D0"), None, income = 10000, tax = 750, lowerBand = Some(14000), upperBand = Some(32000), rate = 20),
        TaxBand(Some("D1"), None, income = 10000, tax = 3000, lowerBand = Some(14000), upperBand = Some(100000), rate = 40),
        TaxBand(Some("D2"), None, income = 40000, tax = 3000, lowerBand = Some(100000), upperBand = Some(200000), rate = 45),
        TaxBand(Some("D3"), None, income = 40000, tax = 3000, lowerBand = Some(200000), upperBand = Some(0), rate = 60)
      )

      val bands = List(
        Band("TaxFree", 5, "0%", 10000, 0, "pa"),
        Band("TaxFree", 5, "0%", 10000, 0, "SR"),
        Band("Band", 50, "Check in more detail", 100000, 9750, "TaxedIncome")
      )

      val taxObjects: Map[TaxObject.Type.Value, TaxDetail] = Map({
        TaxObject.Type.BankInterest -> TaxDetail(taxBands = Some(taxBand))
      })
      val taxAccount = TaxAccount(None, None, tax = 1000,
        taxObjects = taxObjects)
      val accounts = List(AnnualAccount(TaxYear(2016), Some(taxAccount)))
      val testTaxSummary = TaxSummaryDetails(nino = "", version = 0, accounts = accounts)

      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(testTaxSummary)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 200000, 120000, 10, 20000, 60, 9750)
    }
  }
}