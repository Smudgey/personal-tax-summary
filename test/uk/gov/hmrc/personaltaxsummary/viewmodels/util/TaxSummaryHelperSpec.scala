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

package uk.gov.hmrc.personaltaxsummary.viewmodelfactories.util

import data.TaiTestData
import org.joda.time.{DateTime, LocalDate}
import play.api.Play.current
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._
import uk.gov.hmrc.model.nps2.Income.{EmploymentStatus, IncomeType}
import uk.gov.hmrc.model.nps2.{Income, NpsEmployment, TaxAccount}
import uk.gov.hmrc.model.tai.{Employment, TaxYear}
import uk.gov.hmrc.personaltaxsummary.config.StubApplicationConfiguration
import uk.gov.hmrc.personaltaxsummary.viewmodelfactories.util.TaxSummaryHelper.dates
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}
import uk.gov.hmrc.model.{IabdSummary, TaxBand, TaxComponent, rti}
import uk.gov.hmrc.model.rti.RtiEmployment

class TaxSummaryHelperSpec extends UnitSpec with WithFakeApplication with StubApplicationConfiguration with TaiTestData {


  "Dates Object" should {
    "Format time Stamp " in {
      val result = dates.formatEasyReadingTimeStamp(Some(new DateTime(2015, 3, 3, 0 , 0)), "")
      result shouldBe "12:00am, Tuesday 3 March 2015"
    }
  }

  "displaySplitAllowanceMessage" should {
    "display split allowance higher income message when main source of income is more than tax-free amount" in {
      TaxSummaryHelper.displaySplitAllowanceMessage(true, BigDecimal(10000), BigDecimal(4000)).map { splitMessage =>
        splitMessage._1 shouldBe Messages("tai.split.allowance.income.greater.message.p1")
        splitMessage._2.get shouldBe Messages("tai.split.allowance.income.greater.message.p2")
        splitMessage._3.get shouldBe Messages("tai.split.allowance.income.greater.message.p3")
      }
    }

    "display split allowance generic message when main source of income is less than tax-free amount" in {
      TaxSummaryHelper.displaySplitAllowanceMessage(true, BigDecimal(2000), BigDecimal(4000)).map { splitMessage =>
        splitMessage._1 shouldBe Messages("tai.split.allowance.income.less.message")
        splitMessage._2 shouldBe None
        splitMessage._3 shouldBe None
      }
    }

    "display no split allowance message when tax free allowance is not split" in {
      val message = TaxSummaryHelper.displaySplitAllowanceMessage(false, BigDecimal(10000), BigDecimal(4000))
      message shouldBe None
    }
  }

  "displayZeroTaxRateMessage " should {

    "display less than basic dividend income message when dividend income is less than 5000 " in {
      val taxBands = List(TaxBand(lowerBand = Some(BigDecimal(0)), upperBand = Some(BigDecimal(5000)), income = None, rate = Some(BigDecimal(0))))

      val dividends = TaxComponent(amount = BigDecimal(4000), componentType = 0, description = "", iabdSummaries = List(IabdSummary(iabdType = 76, description = "UK Dividend", amount = 4000)))
      val message = TaxSummaryHelper.displayZeroTaxRateMessage(Some(dividends), Some(taxBands))

      message shouldBe Some(Messages("tai.estimatedIncome.ukdividends.lessThanOrEqualToBasic", BigDecimal(5000)))
    }

    "display equal to basic dividend income message when dividend income is equal to 5000 " in {
      val taxBands = List(TaxBand(lowerBand = Some(BigDecimal(0)), upperBand = Some(BigDecimal(5000)), income = None, rate = Some(BigDecimal(0))),
        TaxBand(lowerBand = Some(BigDecimal(5000)), upperBand = Some(BigDecimal(32000)), rate = Some(BigDecimal(7.5)), income = None))

      val dividends = TaxComponent(amount = BigDecimal(5000), componentType = 0, description = "", iabdSummaries = List(IabdSummary(iabdType = 76, description = "UK Dividend", amount = 5000)))

      val message = TaxSummaryHelper.displayZeroTaxRateMessage(Some(dividends), Some(taxBands))

      message shouldBe Some(Messages("tai.estimatedIncome.ukdividends.lessThanOrEqualToBasic", BigDecimal(5000)))
    }

    "display more than basic dividend income message when dividend income is greater than 5000" in {
      val taxBands = List(TaxBand(lowerBand = Some(BigDecimal(0)), upperBand = Some(BigDecimal(5000)), income = None, rate = Some(BigDecimal(0))),
        TaxBand(lowerBand = Some(BigDecimal(5000)), upperBand = Some(BigDecimal(32000)), rate = Some(BigDecimal(7.5)), income = Some(BigDecimal(27000))),
        TaxBand(lowerBand = Some(BigDecimal(32000)), upperBand = Some(BigDecimal(150000)), rate = Some(BigDecimal(32.5)), income = Some(BigDecimal(37000))),
        TaxBand(lowerBand = Some(BigDecimal(150000)), upperBand = Some(BigDecimal(0)), rate = Some(BigDecimal(38.1)), income = None))

      val dividends = TaxComponent(amount = BigDecimal(70000), componentType = 0, description = "", iabdSummaries = List(IabdSummary(iabdType = 76, description = "UK Dividend", amount = 70000)))

      val message = TaxSummaryHelper.displayZeroTaxRateMessage(Some(dividends), Some(taxBands))

      message shouldBe Some(Messages("tai.estimatedIncome.ukdividends.moreThanBasic", BigDecimal(5000), BigDecimal(5000), "7.5% and 32.5%"))
    }

    "display more than basic dividend income message when dividend income is greater than 5000 for an additional higher rate tax payer" in {
      val taxBands = List(TaxBand(lowerBand = Some(BigDecimal(0)), upperBand = Some(BigDecimal(5000)), income = None, rate = Some(BigDecimal(0))),
        TaxBand(lowerBand = Some(BigDecimal(5000)), upperBand = Some(BigDecimal(32000)), rate = Some(BigDecimal(7.5)), income = Some(BigDecimal(27000))),
        TaxBand(lowerBand = Some(BigDecimal(32000)), upperBand = Some(BigDecimal(150000)), rate = Some(BigDecimal(32.5)), income = Some(BigDecimal(37000))),
        TaxBand(lowerBand = Some(BigDecimal(150000)), upperBand = Some(BigDecimal(0)), rate = Some(BigDecimal(38.1)), income = Some(BigDecimal(20000))))

      val dividends = TaxComponent(amount = BigDecimal(170000), componentType = 0, description = "", iabdSummaries = List(IabdSummary(iabdType = 76, description = "UK Dividend", amount = 170000)))

      val message = TaxSummaryHelper.displayZeroTaxRateMessage(Some(dividends), Some(taxBands))

      message shouldBe Some(Messages("tai.estimatedIncome.ukdividends.moreThanBasic", BigDecimal(5000), BigDecimal(5000), "7.5%, 32.5% and 38.1%"))
    }

    "display no message if there are no tax bands " in {
      val dividends = TaxComponent(amount = BigDecimal(70000), componentType = 0, description = "", iabdSummaries = List(IabdSummary(iabdType = 76, description = "UK Dividend", amount = 70000)))

      val message = TaxSummaryHelper.displayZeroTaxRateMessage(Some(dividends), None)

      message shouldBe None
    }

    "display no message if there are no dividends " in {
      val taxBands = List(TaxBand(lowerBand = Some(BigDecimal(0)), upperBand = Some(BigDecimal(5000)), income = None, rate = Some(BigDecimal(0))),
        TaxBand(lowerBand = Some(BigDecimal(5000)), upperBand = Some(BigDecimal(32000)), rate = Some(BigDecimal(7.5)), income = Some(BigDecimal(27000))),
        TaxBand(lowerBand = Some(BigDecimal(32000)), upperBand = Some(BigDecimal(150000)), rate = Some(BigDecimal(32.5)), income = Some(BigDecimal(37000))),
        TaxBand(lowerBand = Some(BigDecimal(150000)), upperBand = Some(BigDecimal(0)), rate = Some(BigDecimal(38.1)), income = None))

      val message = TaxSummaryHelper.displayZeroTaxRateMessage(None, Some(taxBands))

      message shouldBe None
    }
  }

  "getMatchingNpsEmployments " should {

    "return one matching employment with the same works number if more than one nps employments are with the same paye ref and tax district number " in {

      val rtiEmp1 = RtiEmployment("123", "payeRef", "", Nil, Some("worksNumber"), 3)
      val rtiDataTemp = rti.RtiData("", TaxYear(2016), "", List(rtiEmp1))

      val empRecord = Some(NpsEmployment(employerName = Some("empName"), worksNumber = Some("worksNo"), isPrimary = true,
        sequenceNumber = 3, districtNumber = 234, cessationPay = Some(0), start = new LocalDate(1016, 1, 3)))

      val income1 = Income(employmentId = Some(1) , taxDistrict = Some(123), name = "EmpName", isPrimary = true, incomeType = IncomeType.Employment,
        status = EmploymentStatus.Live, payeRef = "payeRef", worksNumber = Some("worksNumber"), taxCode = "taxCode",
        potentialUnderpayment = 0, employmentRecord = empRecord)

      val income2 = Income(employmentId = Some(1) , taxDistrict = Some(123), name = "EmpName", isPrimary = true, incomeType = IncomeType.Employment,
        status = EmploymentStatus.Live, payeRef = "payeRef", worksNumber = Some("worksNumber1"), taxCode = "taxCode",
        potentialUnderpayment = 0, employmentRecord = empRecord)

      val nps = TaxAccount(id = Some(1), date= Some(new LocalDate(2016,1,3)), tax = 1345, incomes = Seq(income1, income2))

      val result = TaxSummaryHelper.getMatchingNpsEmployments(Some(rtiDataTemp), Some(nps))

      result.size shouldBe 1
      result shouldBe List(Employment(income1, Some(rtiEmp1)))
    }

    "return no matching employment if there is no matching employments with the same paye ref and tax district number " in {
      val rtiEmp1 = RtiEmployment("123", "payeRef", "", Nil, Some("worksNumber"), 3)

      val rtiDataTemp = rti.RtiData("", TaxYear(2016), "", List(rtiEmp1))

      val empRecord = Some(NpsEmployment(employerName = Some("empName"), worksNumber = Some("worksNo"), isPrimary = true,
        sequenceNumber = 3, districtNumber = 234, cessationPay = Some(0), start = new LocalDate(1016, 1, 3)))

      val income1 = Income(employmentId = Some(1) , taxDistrict = Some(1), name = "EmpName", isPrimary = true, incomeType = IncomeType.Employment,
        status = EmploymentStatus.Live, payeRef = "payeRef", worksNumber = Some("worksNumber"), taxCode = "taxCode",
        potentialUnderpayment = 0, employmentRecord = empRecord)

      val income2 = Income(employmentId = Some(1) , taxDistrict = Some(12), name = "EmpName", isPrimary = true, incomeType = IncomeType.Employment,
        status = EmploymentStatus.Live, payeRef = "payeRef1", worksNumber = Some("worksNumber1"), taxCode = "taxCode",
        potentialUnderpayment = 0, employmentRecord = empRecord)

      val nps = TaxAccount(id = Some(1), date= Some(new LocalDate(2016,1,3)), tax = 1345, incomes = Seq(income1, income2))

      val result = TaxSummaryHelper.getMatchingNpsEmployments(Some(rtiDataTemp), Some(nps))

      result.size shouldBe 0
      result shouldBe Nil
    }

    "return no matching employment if there is matching employments with the same paye ref and tax district number but no matching works number " in {

      val rtiDataTemp = rti.RtiData("", TaxYear(2016), "", List(RtiEmployment("123", "payeRef", "", Nil, Some("currentPayId"), 3)))

      val empRecord = Some(NpsEmployment(employerName = Some("empName"), worksNumber = Some("worksNo"), isPrimary = true, sequenceNumber = 3, districtNumber = 234, cessationPay = Some(0), start = new LocalDate(1016, 1, 3)))

      val income1 = Income(employmentId = Some(1) , taxDistrict = Some(123), name = "EmpName", isPrimary = true, incomeType = IncomeType.Employment,
        status = EmploymentStatus.Live, payeRef = "payeRef", worksNumber = Some("worksNumber"), taxCode = "taxCode",
        potentialUnderpayment = 0, employmentRecord = empRecord)

      val income2 = Income(employmentId = Some(1) , taxDistrict = Some(123), name = "EmpName", isPrimary = true, incomeType = IncomeType.Employment,
        status = EmploymentStatus.Live, payeRef = "payeRef", worksNumber = Some("worksNumber1"), taxCode = "taxCode",
        potentialUnderpayment = 0, employmentRecord = empRecord)

      val nps = TaxAccount(id = Some(1), date= Some(new LocalDate(2016,1,3)), tax = 1345, incomes = Seq(income1, income2))

      val result = TaxSummaryHelper.getMatchingNpsEmployments(Some(rtiDataTemp), Some(nps))

      result.size shouldBe 0
      result shouldBe Nil
    }

    "return one employment with the same paye ref, tax district number and works number " in {

      val rtiEmp1 = RtiEmployment("123", "payeRef", "", Nil, Some("currentPayId"), 3)

      val rtiDataTemp = rti.RtiData("", TaxYear(2016), "", List(rtiEmp1))

      val empRecord = Some(NpsEmployment(employerName = Some("empName"), worksNumber = Some("worksNo"), isPrimary = true, sequenceNumber = 3, districtNumber = 234, cessationPay = Some(0), start = new LocalDate(1016, 1, 3)))

      val income = Income(employmentId = Some(1) , taxDistrict = Some(123), name = "EmpName", isPrimary = true, incomeType = IncomeType.Employment,
        status = EmploymentStatus.Live, payeRef = "payeRef", worksNumber = Some("worksNumber"), taxCode = "taxCode",
        potentialUnderpayment = 0, employmentRecord = empRecord)

      val nps = TaxAccount(id = Some(1), date= Some(new LocalDate(2016,1,3)), tax = 1345, incomes = Seq(income))

      val result = TaxSummaryHelper.getMatchingNpsEmployments(Some(rtiDataTemp), Some(nps))

      result.size shouldBe 1
      result shouldBe List(Employment(income, Some(rtiEmp1)))
    }

    "return all matching employments with the same paye ref, tax district number and works number " in {

      val rtiEmp1 = RtiEmployment("123", "payeRef", "", Nil, Some("worksNumber"), 3)
      val rtiEmp2 = RtiEmployment("1234", "payeRef1", "", Nil, Some("worksNumber1"), 3)

      val rtiDataTemp = rti.RtiData("", TaxYear(2016), "", List(rtiEmp1, rtiEmp2))

      val empRecord = Some(NpsEmployment(employerName = Some("empName"), worksNumber = Some("worksNo"),
        isPrimary = true, sequenceNumber = 3, districtNumber = 234, cessationPay = Some(0), start = new LocalDate(1016, 1, 3)))

      val income1 = Income(employmentId = Some(1) , taxDistrict = Some(123), name = "EmpName", isPrimary = true, incomeType = IncomeType.Employment,
        status = EmploymentStatus.Live, payeRef = "payeRef", worksNumber = Some("worksNumber"), taxCode = "taxCode",
        potentialUnderpayment = 0, employmentRecord = empRecord)

      val income2 = Income(employmentId = Some(1) , taxDistrict = Some(1234), name = "EmpName", isPrimary = true, incomeType = IncomeType.Employment,
        status = EmploymentStatus.Live, payeRef = "payeRef1", worksNumber = Some("worksNumber1"), taxCode = "taxCode",
        potentialUnderpayment = 0, employmentRecord = empRecord)

      val income3 = Income(employmentId = Some(1) , taxDistrict = Some(123), name = "EmpName", isPrimary = true, incomeType = IncomeType.Employment,
        status = EmploymentStatus.Live, payeRef = "payeRef", worksNumber = Some("worksNumber3"), taxCode = "taxCode",
        potentialUnderpayment = 0, employmentRecord = empRecord)

      val nps = TaxAccount(id = Some(1), date= Some(new LocalDate(2016,1,3)), tax = 1345, incomes = Seq(income1, income2))

      val result = TaxSummaryHelper.getMatchingNpsEmployments(Some(rtiDataTemp), Some(nps))

      result.size shouldBe 2
      result shouldBe List(Employment(income1, Some(rtiEmp1)), Employment(income2, Some(rtiEmp2)))
    }
  }
}
