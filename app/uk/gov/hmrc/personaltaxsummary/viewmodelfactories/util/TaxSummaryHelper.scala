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

import com.ibm.icu.text.SimpleDateFormat
import uk.gov.hmrc.model.nps2.TaxAccount
import org.joda.time.DateTime
import play.api.http.Status
import play.api.Play.current
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._
import uk.gov.hmrc.model.tai.Employment
import uk.gov.hmrc.play.views.helpers.MoneyPounds
import uk.gov.hmrc._
import uk.gov.hmrc.model._
import uk.gov.hmrc.model.rti.RtiStatus

object TaxSummaryHelper {

  def isNotCyPy(taxSummaryDetails: TaxSummaryDetails): Boolean = {

    val previousAnnualAccount = taxSummaryDetails.previousYearAccount
    val currentAnnualAccount = taxSummaryDetails.currentYearAccounts
    val previousNpsEmployments = getMatchingNpsEmployments(previousAnnualAccount.flatMap(_.rti), currentAnnualAccount.flatMap(_.nps))

    val isNotPy = (previousAnnualAccount.flatMap(account => account.rtiStatus).
      getOrElse(RtiStatus(Status.NOT_FOUND, "Data Not Found")).status, previousNpsEmployments.nonEmpty) match {

      case (Status.NOT_FOUND, _) => true
      case (Status.OK, false) => true
      case _ => false
    }

    val isNotCy = (currentAnnualAccount.flatMap(account => account.rtiStatus).
      getOrElse(RtiStatus(Status.NOT_FOUND, "Data Not Found")).status, currentAnnualAccount.exists(_.employments.nonEmpty)) match {
      case (Status.NOT_FOUND, _) => true
      case (Status.OK, false) => true
      case _ => false
    }

    isNotPy && isNotCy
  }

  def getMatchingNpsEmployments(rtiData: Option[rti.RtiData], nps: Option[TaxAccount]) : List[Employment]= {
    val rtiEmps = rtiData.map{_.employments}.getOrElse(Nil)
    val npsIncomes = nps.map(_.incomes.filter(_.employmentRecord.isDefined)).getOrElse(Seq())
    rtiEmps.flatMap{ rtiData =>
      npsIncomes.filter { npsData =>
        npsData.payeRef == rtiData.payeRef &&
          npsData.taxDistrict.contains(rtiData.officeRefNo.toInt)
      } match {
        case Seq(oneEmp) => Some(Employment(oneEmp,Some(rtiData)))
        case Nil => None
        case multipleEmp => multipleEmp.find(_.worksNumber == rtiData.currentPayId &&
          rtiData.currentPayId.isDefined).map{
          nps => Employment(nps,Some(rtiData))
        }

      }
    }
  }

  def sortedTaxableIncomes(incomes: TaxCodeIncomes): List[TaxCodeIncomeSummary] = {
    val taxableIncomes = incomes.employments.map(_.taxCodeIncomes).getOrElse(Nil) :::
      incomes.occupationalPensions.map(_.taxCodeIncomes).getOrElse(Nil) :::
      incomes.taxableStateBenefitIncomes.map(_.taxCodeIncomes).getOrElse(Nil) :::
      incomes.ceasedEmployments.map(_.taxCodeIncomes).getOrElse(Nil)

    taxableIncomes.sortBy(employment => (employment.employmentType.getOrElse(0) * 1000) + employment.employmentId.getOrElse(0))
  }


  def getEditableIncomes(incomes: Option[Incomes]): List[TaxCodeIncomeSummary] = {
    val allIncomes = incomes.map { payeIncomes =>
      sortedTaxableIncomes(payeIncomes.taxCodeIncomes)
    }
    allIncomes.map(_.filter(_.isEditable)).getOrElse(Nil)
  }


  def hasNoTaxableIncome(taxSummaryDetails: model.TaxSummaryDetails): Boolean = {
    taxSummaryDetails.increasesTax.map(_.total).getOrElse(BigDecimal(0)) > taxSummaryDetails.decreasesTax.map(_.total).getOrElse(BigDecimal(0))
  }


  def getPPR(taxSummaryDetails: model.TaxSummaryDetails): (BigDecimal, BigDecimal) = {

    val pprSource = taxSummaryDetails.extensionReliefs.flatMap(extensionRelief =>
      extensionRelief.personalPension.map(_.sourceAmount)
    ).getOrElse(BigDecimal(0))

    val pprRelief = taxSummaryDetails.extensionReliefs.flatMap(extensionRelief =>
      extensionRelief.personalPension.map(_.reliefAmount)
    ).getOrElse(BigDecimal(0))

    (pprSource, pprRelief)
  }

  def getGiftAid(taxSummaryDetails: model.TaxSummaryDetails): (BigDecimal, BigDecimal) = {

    val giftAidSource = taxSummaryDetails.extensionReliefs.flatMap(extensionRelief =>
      extensionRelief.giftAid.map(_.sourceAmount)
    ).getOrElse(BigDecimal(0))

    val giftAidRelief = taxSummaryDetails.extensionReliefs.flatMap(extensionRelief =>
      extensionRelief.giftAid.map(_.reliefAmount)
    ).getOrElse(BigDecimal(0))

    (giftAidSource, giftAidRelief)
  }


  def hasMultipleIncomes(details: model.TaxSummaryDetails): Boolean = {
    getEditableIncomes(details.increasesTax.flatMap(_.incomes)).size > 1
  }

  def getTaxablePayYTD(details: model.TaxSummaryDetails, employerId: BigDecimal): BigDecimal = {
    val incomeExplanations = details.incomeData.map(x => x.incomeExplanations)

    val taxablePayYTD: BigDecimal = incomeExplanations match {
      case Some(incomeExplanations) =>
        val income = incomeExplanations.find(_.incomeId == employerId)
        income.map(_.payToDate).getOrElse(BigDecimal(0))
      case _ => BigDecimal(0)
    }
    taxablePayYTD
  }

  def getSingularIncomeId(details: model.TaxSummaryDetails): Option[Int] = {
    val editableIncomes = getEditableIncomes(details.increasesTax.flatMap(_.incomes))
    if (editableIncomes.size == 1) {
      editableIncomes.flatMap { income =>
        income.employmentId
      }.headOption
    } else {
      None
    }
  }


  //method to decide if the user can see info about cy+1 or not. Can be updated to include Gatekeeper rules
  def cyPlusOneAvailable(taxSummaryDetals: TaxSummaryDetails): Boolean = {
    taxSummaryDetals.cyPlusOneChange.isDefined
  }


  object dates {

    private def easyReadingTimeStampFormat = new SimpleDateFormat("h:mmaa")

    private def easyReadingDateFormat = new SimpleDateFormat("EEEE d MMMM yyyy")

    def formatEasyReadingTimeStamp(date: Option[DateTime], default: String) =
      date match {
        case Some(d) =>
          val time = easyReadingTimeStampFormat.format(d.toDate).toLowerCase
          val date = easyReadingDateFormat.format(d.toDate)
          s"$time, $date"
        case None => default
      }
  }

  implicit def strToMoneyPounds(str: String): MoneyPounds = {
    MoneyPounds(BigDecimal(str))
  }


  def displaySplitAllowanceMessage(splitAllowance: Boolean, primaryIncome: BigDecimal,
                                   taxFreeAmount: BigDecimal): Option[(String, Option[String], Option[String])] = {
    splitAllowance match {
      case true if primaryIncome > taxFreeAmount => Some((Messages("tai.split.allowance.income.greater.message.p1"),
        Some(Messages("tai.split.allowance.income.greater.message.p2")), Some(Messages("tai.split.allowance.income.greater.message.p3"))))
      case true => Some((Messages("tai.split.allowance.income.less.message"), None, None))
      case _ => None
    }
  }


  def displayZeroTaxRateMessage(dividends: Option[TaxComponent], taxBands: Option[List[TaxBand]]): Option[String] = {

    dividends.map(_.iabdSummaries).flatMap(_.find(_.iabdType == TaiConstants.IABD_TYPE_UKDIVIDENDS)).flatMap { ukDiv =>
      val ukDivTotalIncome = ukDiv.amount

      taxBands.flatMap {
        taxBands =>
          val taxFreeDividend = TaiConstants.TAX_FREE_DIVIDEND

          val higherTaxRates = taxBands.filter { taxBand =>
            taxBand.income.getOrElse(BigDecimal(0)) > 0 && taxBand.rate.getOrElse(BigDecimal(0)) > 0
          }.map(_.rate).map(_.mkString("", "% ", "%"))

          if (ukDivTotalIncome <= taxFreeDividend) {
            Some(Messages("tai.estimatedIncome.ukdividends.lessThanOrEqualToBasic", MoneyPounds(taxFreeDividend, 0).quantity))

          } else if ((ukDivTotalIncome > taxFreeDividend) && higherTaxRates.nonEmpty) {
            Some(Messages("tai.estimatedIncome.ukdividends.moreThanBasic", MoneyPounds(taxFreeDividend, 0).quantity, MoneyPounds(taxFreeDividend, 0).quantity,
              if (higherTaxRates.size > 1) {
                higherTaxRates.take(higherTaxRates.size - 1).mkString(", ") + " and " + higherTaxRates.last
              } else {
                higherTaxRates.head
              }))

          } else {
            None
          }

      }
    }
  }

}