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
import uk.gov.hmrc.model.{IabdSummary, TaxComponent, TaxSummaryDetails, nps2}
import uk.gov.hmrc.personaltaxsummary.config.StubApplicationConfiguration
import uk.gov.hmrc.personaltaxsummary.viewmodelfactories.EstimatedIncomeViewModelFactory
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}
import uk.gov.hmrc.play.views.helpers.MoneyPounds
import WrappedDataMatchers._
import uk.gov.hmrc.model._
import uk.gov.hmrc.personaltaxsummary.domain.PersonalTaxSummaryContainer
import uk.gov.hmrc.personaltaxsummary.utils.NinoGenerator

class EstimatedIncomeViewModelFactorySpec extends UnitSpec with WithFakeApplication with StubApplicationConfiguration with TaiTestData {

  "EstimatedIncomeViewModelFactory createObject" should {

    val annualAccounts = List(AnnualAccount(TaxYear(2017), Some(TaxAccount(None, None, 1564.45,
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

    "have outstanding debt" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CZ629113A"), outstandingDebtTaxSummary.copy(accounts = annualAccounts))

      result.additionalTaxTable shouldBe List((Messages("tai.taxCalc.OutstandingDebt.title"), MoneyPounds(200, 2).quantity))
    }

    "have child benefit" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CZ629113A"), everythingTaxSummary.copy(accounts = annualAccounts))

      result.additionalTaxTable.contains((Messages("tai.taxCalc.childBenefit.title"), MoneyPounds(1500, 2).quantity)) shouldBe true
    }

    "have in year adjustment" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CZ629113A"), everythingTaxSummary.copy(accounts = annualAccounts))

      result.additionalTaxTable.contains((Messages("tai.taxcode.deduction.type-45"), MoneyPounds(350, 2).quantity)) shouldBe true
    }

    "not return a zero income tax estimate message as reductions are less than the income tax due" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CN499213B"), reductionsLessThanIncomeTaxLiabilityTaxSummary)

      result.incomeTaxEstimate shouldBe 1
      result.incomeTaxReducedToZeroMessage.isDefined shouldBe false
    }

    "return a zero income tax estimate message as reductions are greater than the income tax due" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CN499213B"), reductionsGreaterThanIncomeTaxLiabilityTaxSummary)

      result.incomeTaxEstimate shouldBe 0
      result.reductionsTable.size shouldBe 2
      result.incomeTaxReducedToZeroMessage.isDefined shouldBe true
    }

    "return a zero income tax estimate message as reductions are equal to the income tax due" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CN499213B"), reductionsEqualToIncomeTaxLiabilityTaxSummary)

      result.incomeTaxEstimate shouldBe 0
      result.reductionsTable.size shouldBe 2
      result.incomeTaxReducedToZeroMessage.isDefined shouldBe true
    }

    "have 'hasSSR' flag as true for SR band" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CN499213B"), containsSRBandTaxSummary)

      result.hasSSR shouldBe true
      result.hasPSA shouldBe false
    }

    "have 'hasPSA' flag as true for PSR band" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CN499213B"), containsPSRBandTaxSummary)

      result.hasPSA shouldBe true
      result.hasSSR shouldBe false
    }

    "have all the ukdividend bands list" in {
      val result = EstimatedIncomeViewModelFactory.createObject(Nino("CN499213B"), ukDividendsTaxSummary)

      result.ukDividends shouldBe Some(TaxComponent(20000,0, "",List(IabdSummary(76,"UK Dividend",20000,Some(0),None))))
      result.taxBands shouldBe Some(List(TaxBand(Some("SDR"),None,0,0,Some(0),Some(5000),0),
        TaxBand(Some("LDR"),None,22,4.4,Some(5000),Some(32910),7.5),
        TaxBand(Some("HDR1"),None,0,0,Some(32910),Some(151125),32.5),
        TaxBand(Some("HDR2"),None,0,0,Some(151125),Some(0),38.1)))
    }
  }

  "merge Tax bands" should {

    "return None when an empty list is supplied." in {

      val result = EstimatedIncomeViewModelFactory.mergedBands(Nil)
      result shouldBe None

    }

    "return empty string in table percentage when tax explanation link is not coming " in {
      val taxBand = List(TaxBand(None, None, income = 1000, tax = 20, lowerBand = None, upperBand = Some(4000), rate = 20),
        TaxBand(None, None, income = 2500, tax = 40, lowerBand = None, upperBand = Some(5000), rate = 20),
        TaxBand(None, None, income = 1000, tax = 20, lowerBand = None, upperBand = Some(4000), rate = 40),
        TaxBand(None, None, income = 2000, tax = 20, lowerBand = None, upperBand = Some(4000), rate = 40))

      val dataF = EstimatedIncomeViewModelFactory.mergedBands(taxBand)
      dataF.get shouldBe Band("Band", 100, "", 6500, 100, "TaxedIncome")
    }

    "return only one merged tax band for other than zero% rate band" in {
      val taxBand = List(TaxBand(None, None, income = 1000, tax = 20, lowerBand = None, upperBand = Some(4000), rate = 20),
        TaxBand(None, None, income = 2500, tax = 40, lowerBand = None, upperBand = Some(5000), rate = 20),
        TaxBand(None, None, income = 1000, tax = 20, lowerBand = None, upperBand = Some(4000), rate = 40),
        TaxBand(None, None, income = 2000, tax = 20, lowerBand = None, upperBand = Some(4000), rate = 40))

      val links = Map("taxExplanationScreen" -> "Check in more detail")
      val dataF = EstimatedIncomeViewModelFactory.mergedBands(taxBand, links = links)
      dataF.get shouldBe Band("Band", 100, "Check in more detail", 6500, 100, "TaxedIncome")
    }
  }

  "individual Tax bands" should {

    "return an empty list when an empty list is supplied." in {

      val result = EstimatedIncomeViewModelFactory.individualBands(Nil)
      result shouldBe Nil

    }

    "return two tax bands for 0% rate" in {
      val taxBand = List(TaxBand(Some("PSA"), None, income = 1000, tax = 0, lowerBand = None, upperBand = Some(5000), rate = 0),
        TaxBand(Some("B"), None, income = 2000, tax = 0, lowerBand = None, upperBand = Some(5000), rate = 0))

      val dataF = EstimatedIncomeViewModelFactory.individualBands(taxBand)
      dataF shouldBe List(Band("TaxFree", 20, "0%", 1000, 0, "PSA"), Band("TaxFree", 40, "0%", 2000, 0, "B"))
    }
  }

  "getUpperBand" should {

    "return 0 when empty list" in {
      val taxBands = Nil
      val upperBand = EstimatedIncomeViewModelFactory.getUpperBand(taxBands)
      upperBand shouldBe 0
    }

    "return default value when only pa band has been passed" in {
      val taxBands: List[TaxBand] = List(TaxBand(Some("pa"), None, income = 11500, tax = 0, lowerBand = None, upperBand = None, rate = 0))
      val upperBand = EstimatedIncomeViewModelFactory.getUpperBand(taxBands)
      upperBand shouldBe 11500
    }

    "return proper upperBand when passed two bands (0&20)" in {
      val taxBands = List(
        TaxBand(Some("pa"), None, income = 3200, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("B"), None, income = 16000, tax = 5000, lowerBand = Some(11000), upperBand = Some(28800), rate = 20)
      )

      val upperBand = EstimatedIncomeViewModelFactory.getUpperBand(taxBands = taxBands)

      upperBand shouldBe 32000
    }

    "return income as upperBand when income is greater than upper band" in {
      val taxBands = List(
        TaxBand(Some("pa"), None, income = 5000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("B"), None, income = 15000, tax = 3000, lowerBand = Some(11000), upperBand = Some(32000), rate = 20),
        TaxBand(Some("D0"), None, income = 150000, tax = 60000, lowerBand = Some(32000), upperBand = Some(150000), rate = 40),
        TaxBand(Some("D1"), None, income = 30000, tax = 2250, lowerBand = Some(150000), upperBand = Some(0), rate = 45)
      )

      val upperBand = EstimatedIncomeViewModelFactory.getUpperBand(taxBands = taxBands)

      upperBand shouldBe 200000
    }

    "deduct personal allowance from tax-free allowances to get the upper-band if user is in higher rate" in {
      val taxBands = List(
        TaxBand(Some("pa"), None, income = 5000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("B"), None, income = 15000, tax = 3000, lowerBand = Some(11000), upperBand = Some(32000), rate = 20),
        TaxBand(Some("D0"), None, income = 100, tax = 60000, lowerBand = Some(32000), upperBand = Some(150000), rate = 40)
      )

      val upperBand = EstimatedIncomeViewModelFactory.getUpperBand(taxBands = taxBands, personalAllowance = Some(5000))

      upperBand shouldBe 150000
    }

    "not deduct personal allowance from tax-free allowances to get the upper-band if user is not in higher rate" in {
      val taxBands = List(
        TaxBand(Some("pa"), None, income = 5000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("B"), None, income = 15000, tax = 3000, lowerBand = Some(11000), upperBand = Some(32000), rate = 20)
      )

      val upperBand = EstimatedIncomeViewModelFactory.getUpperBand(taxBands = taxBands, personalAllowance = Some(5000))

      upperBand shouldBe 37000
    }

  }

  "calculate bar percentage" should {

    "return value 0 when no band has passed" in {

      val percentage = EstimatedIncomeViewModelFactory.calcBarPercentage(20000, Nil)

      percentage shouldBe 0
    }

    "return valid percentage for first income when two bands has passed" in {
      val taxBands = List(
        TaxBand(Some("pa"), None, income = 3200, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("B"), None, income = 16000, tax = 5000, lowerBand = Some(11000), upperBand = Some(28800), rate = 20)
      )

      val percentage = EstimatedIncomeViewModelFactory.calcBarPercentage(3200, taxBands)

      percentage shouldBe 10
    }

    "return valid percentage for second income when two bands has passed" in {
      val taxBands = List(
        TaxBand(Some("pa"), None, income = 3200, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("B"), None, income = 16000, tax = 5000, lowerBand = Some(11000), upperBand = Some(28800), rate = 20)
      )

      val percentage = EstimatedIncomeViewModelFactory.calcBarPercentage(16000, taxBands)

      percentage shouldBe 50
    }

    "return value till two decimal" in {
      val taxBand = List(
        TaxBand(Some("pa"), None, income = 4000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("SR"), None, income = 4000, tax = 0, lowerBand = Some(11000), upperBand = Some(14000), rate = 0),
        TaxBand(Some("D0"), None, income = 15000, tax = 3000, lowerBand = Some(14000), upperBand = Some(32000), rate = 20)
      )

      val percentage = EstimatedIncomeViewModelFactory.calcBarPercentage(15000, taxBand)

      percentage shouldBe 41.66

    }

  }

  "retrieve tax bands" should {

    "return tax bands" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 5000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("B"), None, income = 15000, tax = 3000, lowerBand = Some(11000), upperBand = Some(32000), rate = 20),
        TaxBand(Some("D0"), None, income = 150000, tax = 60000, lowerBand = Some(32000), upperBand = Some(150000), rate = 40),
        TaxBand(Some("D1"), None, income = 30000, tax = 2250, lowerBand = Some(150000), upperBand = Some(0), rate = 45)
      )

      val taxObjects: Map[TaxObject.Type.Value, TaxDetail] = Map({
        TaxObject.Type.BankInterest -> TaxDetail(taxBands = Some(taxBand))
      })
      val taxAccount = TaxAccount(None, None, tax = 1000,
        taxObjects = taxObjects)
      val accounts = List(AnnualAccount(TaxYear(2017), Some(taxAccount)))
      val testTaxSummary = TaxSummaryDetails(nino = "", version = 0, accounts = accounts)

      val taxBands = EstimatedIncomeViewModelFactory.retrieveTaxBands(testTaxSummary)

      taxBands shouldBe taxBand

    }

    "return sorted tax bands" in {
      val taxBand = List(
        TaxBand(Some("pa"), None, income = 5000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("B"), None, income = 15000, tax = 3000, lowerBand = Some(11000), upperBand = Some(32000), rate = 20),
        TaxBand(Some("D1"), None, income = 30000, tax = 2250, lowerBand = Some(150000), upperBand = Some(0), rate = 45),
        TaxBand(Some("D0"), None, income = 150000, tax = 60000, lowerBand = Some(32000), upperBand = Some(150000), rate = 40)
      )

      val taxObjects: Map[TaxObject.Type.Value, TaxDetail] = Map({
        TaxObject.Type.BankInterest -> TaxDetail(taxBands = Some(taxBand))
      })
      val taxAccount = TaxAccount(None, None, tax = 1000,
        taxObjects = taxObjects)
      val accounts = List(AnnualAccount(TaxYear(2017), Some(taxAccount)))
      val testTaxSummary = TaxSummaryDetails(nino = "", version = 0, accounts = accounts)

      val taxBands = EstimatedIncomeViewModelFactory.retrieveTaxBands(testTaxSummary)

      taxBands shouldBe List(
        TaxBand(Some("pa"), None, income = 5000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("B"), None, income = 15000, tax = 3000, lowerBand = Some(11000), upperBand = Some(32000), rate = 20),
        TaxBand(Some("D0"), None, income = 150000, tax = 60000, lowerBand = Some(32000), upperBand = Some(150000), rate = 40),
        TaxBand(Some("D1"), None, income = 30000, tax = 2250, lowerBand = Some(150000), upperBand = Some(0), rate = 45)
      )
    }

    "return merged tax bands" in {
      val taxBand = List(
        TaxBand(Some("pa"), None, income = 5000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("pa"), None, income = 5000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("pa"), None, income = 5000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("pa"), None, income = 5000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("B"), None, income = 15000, tax = 3000, lowerBand = Some(11000), upperBand = Some(32000), rate = 20),
        TaxBand(Some("D1"), None, income = 30000, tax = 2250, lowerBand = Some(150000), upperBand = Some(0), rate = 45),
        TaxBand(Some("D0"), None, income = 150000, tax = 60000, lowerBand = Some(32000), upperBand = Some(150000), rate = 40)
      )

      val taxObjects: Map[TaxObject.Type.Value, TaxDetail] = Map({
        TaxObject.Type.BankInterest -> TaxDetail(taxBands = Some(taxBand))
      })
      val taxAccount = TaxAccount(None, None, tax = 1000,
        taxObjects = taxObjects)
      val accounts = List(AnnualAccount(TaxYear(2017), Some(taxAccount)))
      val testTaxSummary = TaxSummaryDetails(nino = "", version = 0, accounts = accounts)

      val taxBands = EstimatedIncomeViewModelFactory.retrieveTaxBands(testTaxSummary)

      taxBands shouldBe List(
        TaxBand(Some("pa"), None, income = 20000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("B"), None, income = 15000, tax = 3000, lowerBand = Some(11000), upperBand = Some(32000), rate = 20),
        TaxBand(Some("D0"), None, income = 150000, tax = 60000, lowerBand = Some(32000), upperBand = Some(150000), rate = 40),
        TaxBand(Some("D1"), None, income = 30000, tax = 2250, lowerBand = Some(150000), upperBand = Some(0), rate = 45)
      )
    }



  }

  "bandedGraph" should {

    "return an empty BandedGraph with Nil bands and values set to zero when an empty list is supplied." in {

      val result = EstimatedIncomeViewModelFactory.createBandedGraph(Nil)

      result shouldBe BandedGraph("taxGraph", Nil,0,0,0,0,0,0,0, None)

    }

    "have two bands(0&20) to display in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 3200, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("B"), None, income = 16000, tax = 5000, lowerBand = Some(11000), upperBand = Some(28800), rate = 20)
      )

      val bands = List(Band("TaxFree",10.00,"0%",3200,0,"pa"),
        Band("Band",50.00,"20%",16000,5000,"B"))

      val nextBandMessage = Some("You can have £12,800 more before your income reaches the next tax band.")

      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(taxBand)
      dataF shouldBe BandedGraph("taxGraph", bands,0,32000,19200,10.00,3200,60.00,5000, nextBandMessage)
    }

    "have two bands(0 & Taxed Income) to display in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 3000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("B"), None, income = 15000, tax = 3000, lowerBand = Some(11000), upperBand = Some(32000), rate = 20),
        TaxBand(Some("D0"), None, income = 30000, tax = 12000, lowerBand = Some(32000), upperBand = Some(147000), rate = 40)
      )

      val bands = List(
        Band("TaxFree", 2.00, "0%", 3000, 0, "pa"),
        Band("Band", 30.00, "Check in more detail", 45000, 15000, "TaxedIncome")
      )

      val nextBandMessage = Some("You can have £102,000 more before your income reaches the next tax band.")
      val links = Map("taxExplanationScreen" -> "Check in more detail")
      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(taxBand, links= links)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 150000, 48000, 2.00, 3000, 32.00, 15000, nextBandMessage)
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

      val links = Map("taxExplanationScreen" -> "Check in more detail")
      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(taxBand, links= links)
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

      val links = Map("taxExplanationScreen" -> "Check in more detail")
      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(taxBand, links= links)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 200000, 200000, 0, 0, 100, 65250)
    }

    "have two 0 % band and one 20% band in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 4000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("SR"), None, income = 4000, tax = 0, lowerBand = Some(11000), upperBand = Some(14000), rate = 0),
        TaxBand(Some("D0"), None, income = 15000, tax = 3000, lowerBand = Some(14000), upperBand = Some(32000), rate = 20)
      )

      val bands = List(
        Band("TaxFree", 11.11, "0%", 4000, 0, "pa"),
        Band("TaxFree", 11.11, "0%", 4000, 0, "SR"),
        Band("Band", 41.66, "20%", 15000, 3000, "D0")
      )

      val nextBandMessage = Some("You can have £13,000 more before your income reaches the next tax band.")

      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(taxBand)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 36000, 23000, 22.22, 8000, 63.88, 3000, nextBandMessage)
    }

    "have two 0 % band and one Taxed Income band in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 10000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("SR"), None, income = 10000, tax = 0, lowerBand = Some(11000), upperBand = Some(14000), rate = 0),
        TaxBand(Some("B"), None, income = 10000, tax = 3000, lowerBand = Some(14000), upperBand = Some(32000), rate = 20),
        TaxBand(Some("B"), None, income = 10000, tax = 3000, lowerBand = Some(14000), upperBand = Some(30000), rate = 20)
      )

      val bands = List(
        Band("TaxFree", 25, "0%", 10000, 0, "pa"),
        Band("TaxFree", 25, "0%", 10000, 0, "SR"),
        Band("Band", 50, "Check in more detail", 20000, 6000, "TaxedIncome")
      )

      val links = Map("taxExplanationScreen" -> "Check in more detail")
      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(taxBand, links= links)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 40000, 40000, 50, 20000, 100, 6000)
    }

    "have two 0 % band and one 7.5% band in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 11000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("SR"), None, income = 3000, tax = 0, lowerBand = Some(11000), upperBand = Some(14000), rate = 0),
        TaxBand(Some("SDR"), None, income = 15000, tax = 2000, lowerBand = Some(14000), upperBand = Some(18000), rate = 7.5)
      )

      val bands = List(
        Band("TaxFree", 37.93, "0%", 11000, 0, "pa"),
        Band("TaxFree", 10.34, "0%", 3000, 0, "SR"),
        Band("Band", 51.72, "7.5%", 15000, 2000, "SDR")
      )

      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(taxBand)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 29000, 29000, 48.27, 14000, 99.99, 2000)
    }

    "have three 0 % band and zero other band in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 11000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("SR"), None, income = 3000, tax = 0, lowerBand = Some(11000), upperBand = Some(14000), rate = 0),
        TaxBand(Some("SDR"), None, income = 15000, tax = 0, lowerBand = Some(14000), upperBand = Some(32000), rate = 0)
      )

      val bands = List(
        Band("TaxFree", 25.58, "0%", 11000, 0, "pa"),
        Band("TaxFree", 6.97, "0%", 3000, 0, "SR"),
        Band("TaxFree", 34.88, "0%", 15000, 0, "SDR")
      )

      val nextBandMessage = Some("You can have £14,000 more before your income reaches the next tax band.")

      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(taxBand)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 43000, 29000, 67.43, 29000, 67.43, 0, nextBandMessage)
    }

    "have two 0 % band and one Taxed Income band(7.5 & 20 ) in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 10000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("SR"), None, income = 10000, tax = 0, lowerBand = Some(11000), upperBand = Some(14000), rate = 0),
        TaxBand(Some("SDR"), None, income = 10000, tax = 750, lowerBand = Some(14000), upperBand = Some(32000), rate = 7.5),
        TaxBand(Some("B"), None, income = 10000, tax = 3000, lowerBand = Some(14000), upperBand = Some(30000), rate = 20)
      )

      val bands = List(
        Band("TaxFree", 25.00, "0%", 10000, 0, "pa"),
        Band("TaxFree", 25.00, "0%", 10000, 0, "SR"),
        Band("Band", 50.00, "Check in more detail", 20000, 3750, "TaxedIncome")
      )

      val links = Map("taxExplanationScreen" -> "Check in more detail")
      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(taxBand, links= links)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 40000, 40000, 50.00, 20000, 100.00, 3750)
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
        Band("TaxFree", 9.09, "0%", 10000, 0, "pa"),
        Band("TaxFree", 9.09, "0%", 10000, 0, "SR"),
        Band("Band", 36.36, "Check in more detail", 40000, 6750, "TaxedIncome")
      )

      val nextBandMessage = Some("You can have £50,000 more before your income reaches the next tax band.")

      val links = Map("taxExplanationScreen" -> "Check in more detail")
      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(taxBand, links= links)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 110000, 60000, 18.18, 20000, 54.54, 6750, nextBandMessage)
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
        Band("TaxFree", 4.76, "0%", 10000, 0, "pa"),
        Band("TaxFree", 4.76, "0%", 10000, 0, "SR"),
        Band("Band", 47.61, "Check in more detail", 100000, 9750, "TaxedIncome")
      )

      val nextBandMessage = Some("You can have £90,000 more before your income reaches the next tax band.")

      val links = Map("taxExplanationScreen" -> "Check in more detail")
      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(taxBand, links= links)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 210000, 120000, 9.52, 20000, 57.13, 9750, nextBandMessage)
    }

    "have tax-free allowance on the top in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 5000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("pa"), None, income = 5000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("SR"), None, income = 10000, tax = 0, lowerBand = Some(11000), upperBand = Some(14000), rate = 0),
        TaxBand(Some("D0"), None, income = 10000, tax = 750, lowerBand = Some(14000), upperBand = Some(32000), rate = 20),
        TaxBand(Some("D1"), None, income = 10000, tax = 3000, lowerBand = Some(14000), upperBand = Some(100000), rate = 40),
        TaxBand(Some("D2"), None, income = 40000, tax = 3000, lowerBand = Some(100000), upperBand = Some(200000), rate = 45),
        TaxBand(Some("D3"), None, income = 40000, tax = 3000, lowerBand = Some(200000), upperBand = Some(0), rate = 60)
      )

      val bands = List(
        Band("TaxFree", 4.76, "0%", 10000, 0, "pa"),
        Band("TaxFree", 4.76, "0%", 10000, 0, "SR"),
        Band("Band", 47.61, "Check in more detail", 100000, 9750, "TaxedIncome")
      )

      val nextBandMessage = Some("You can have £90,000 more before your income reaches the next tax band.")

      val taxObjects: Map[TaxObject.Type.Value, TaxDetail] = Map({
        TaxObject.Type.BankInterest -> TaxDetail(taxBands = Some(taxBand))
      })
      val taxAccount = TaxAccount(None, None, tax = 1000,
        taxObjects = taxObjects)
      val accounts = List(AnnualAccount(TaxYear(2017), Some(taxAccount)))
      val testTaxSummary = TaxSummaryDetails(nino = "", version = 0, accounts = accounts)
      val links = Map("taxExplanationScreen" -> "Check in more detail")
      val dataF = EstimatedIncomeViewModelFactory.createBandedGraph(EstimatedIncomeViewModelFactory.retrieveTaxBands(testTaxSummary), links= links)
      dataF shouldBe BandedGraph("taxGraph", bands, 0, 210000, 120000, 9.52, 20000, 57.13, 9750, nextBandMessage)
    }

    "return zero for decreasesTax total when there is no decreasesTax value" in {
      val nino = NinoGenerator.randomNino
      val personalTaxSummary = PersonalTaxSummaryContainer(
        details = TaxSummaryDetails(
          nino = nino.toString,
          version = 0,
          totalLiability = Some(TotalLiability(
            totalTax = BigDecimal("0"))),
          increasesTax = Some(IncreasesTax(total = BigDecimal("0")))),
        links = Map.empty)
      val result = EstimatedIncomeViewModelFactory.createObject(nino, personalTaxSummary)

      result.taxFreeEstimate shouldBe BigDecimal("0")
    }

    "return the correct value for decreasesTax total" in {
      val nino = NinoGenerator.randomNino
      val decreasesTaxTotalValue = BigDecimal("2000")
      val personalTaxSummary = PersonalTaxSummaryContainer(
        details = TaxSummaryDetails(
          nino = nino.toString,
          version = 0,
          totalLiability = Some(TotalLiability(
            totalTax = BigDecimal("0"))),
          decreasesTax = Some(DecreasesTax(total = decreasesTaxTotalValue)),
          increasesTax = Some(IncreasesTax(total = BigDecimal("0")))),
        links = Map.empty)
      val result = EstimatedIncomeViewModelFactory.createObject(nino, personalTaxSummary)

      result.taxFreeEstimate shouldBe decreasesTaxTotalValue
    }
    
  }

  "oldBandedGraph" should {

    "return an empty BandedGraph with Nil bands and values set to zero when an empty list is supplied." in {

      val result = EstimatedIncomeViewModelFactory.createBandedGraphWithBandsOnly(Nil)

      result shouldBe BandedGraph("taxGraph", Nil,0,0,0,0,0,0,0, None)

    }

    "have two bands(0&20) to display in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 3200, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("B"), None, income = 16000, tax = 5000, lowerBand = Some(11000), upperBand = Some(28800), rate = 20)
      )

      val bands = List(Band("",barPercentage = 0,"0",3200,0,"pa"),
        Band("",barPercentage = 0,"20",16000,5000,"B"))

      val dataF = EstimatedIncomeViewModelFactory.createBandedGraphWithBandsOnly(taxBand)
      dataF shouldBe BandedGraph("taxGraph", bands, incomeTotal = 19200, taxTotal = 5000 )
    }

    "have three bands 0, 20 & 40 to display in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 3000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("B"), None, income = 15000, tax = 3000, lowerBand = Some(11000), upperBand = Some(32000), rate = 20),
        TaxBand(Some("D0"), None, income = 30000, tax = 12000, lowerBand = Some(32000), upperBand = Some(147000), rate = 40)
      )

      val bands = List(
        Band("", barPercentage = 0, "0", 3000, 0, "pa"),
        Band("", barPercentage = 0, "20", 15000, 3000, "B"),
        Band("", barPercentage = 0, "40", 30000, 12000, "D0")
      )

      val dataF = EstimatedIncomeViewModelFactory.createBandedGraphWithBandsOnly(taxBand)
      dataF shouldBe BandedGraph("taxGraph", bands, incomeTotal = 48000, taxTotal = 15000 )
    }

    "have multiple rate band 0,7.5 & 20 & 45 & 60 in graph" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 10000, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(Some("SR"), None, income = 10000, tax = 0, lowerBand = Some(11000), upperBand = Some(14000), rate = 0),
        TaxBand(Some("D0"), None, income = 10000, tax = 750, lowerBand = Some(14000), upperBand = Some(32000), rate = 20),
        TaxBand(Some("D1"), None, income = 10000, tax = 3000, lowerBand = Some(14000), upperBand = Some(100000), rate = 40),
        TaxBand(Some("D2"), None, income = 40000, tax = 3000, lowerBand = Some(100000), upperBand = Some(200000), rate = 45),
        TaxBand(Some("D3"), None, income = 40000, tax = 3000, lowerBand = Some(200000), upperBand = Some(0), rate = 60)
      )

      val bands = List(
        Band("", barPercentage = 0, "0", 10000, 0, "pa"),
        Band("", barPercentage = 0, "0", 10000, 0, "SR"),
        Band("", barPercentage = 0, "20", 10000, 750, "D0"),
        Band("", barPercentage = 0, "40", 10000, 3000, "D1"),
        Band("", barPercentage = 0, "45", 40000, 3000, "D2"),
        Band("", barPercentage = 0, "60", 40000, 3000, "D3")
      )

      val dataF = EstimatedIncomeViewModelFactory.createBandedGraphWithBandsOnly(taxBand)
      dataF shouldBe BandedGraph("taxGraph",bands, incomeTotal = 120000, taxTotal = 9750 )
    }

    "return NA as band type if no band-type coming in Tax-Bands model" in {

      val taxBand = List(
        TaxBand(Some("pa"), None, income = 3200, tax = 0, lowerBand = Some(0), upperBand = Some(11000), rate = 0),
        TaxBand(None, None, income = 16000, tax = 5000, lowerBand = Some(11000), upperBand = Some(28800), rate = 20)
      )

      val bands = List(Band("",barPercentage = 0,"0",3200,0,"pa"),
        Band("",barPercentage = 0,"20",16000,5000,Messages("tai.not-applicable")))

      val dataF = EstimatedIncomeViewModelFactory.createBandedGraphWithBandsOnly(taxBand)
      dataF shouldBe BandedGraph("taxGraph", bands, incomeTotal = 19200, taxTotal = 5000 )
    }

  }

  "fetchPotentialUnderpayment" should {

    "return false if tax details include iya cy and iya cy plus one" in {
      val result = EstimatedIncomeViewModelFactory.fetchPotentialUnderpayment(potentialUnderpaymentTaxSummary)
      result shouldBe false
    }

    "return false if tax details doesn't include iya cy or iya cy plus one" in {
      val result = EstimatedIncomeViewModelFactory.fetchPotentialUnderpayment(potentialUnderpaymentTaxSummaryNoIya)
      result shouldBe false
    }

    "return false if tax details includes iya cy but doesn't include iya cy plus one" in {
      val result = EstimatedIncomeViewModelFactory.fetchPotentialUnderpayment(potentialUnderpaymentTaxSummaryIyaCY)
      result shouldBe false
    }

    "return true if tax details doesn't include iya cy but does include iya cy plus one" in {
      val result = EstimatedIncomeViewModelFactory.fetchPotentialUnderpayment(potentialUnderpaymentTaxSummaryIyaCYPlusOne)
      result shouldBe true
    }

  }

}