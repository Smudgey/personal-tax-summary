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

package uk.gov.hmrc.personaltaxsummary.viewmodelfactories

import play.api.Play.current
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.model.nps2.TaxBand
import uk.gov.hmrc.model.tai.TaxYear
import uk.gov.hmrc.model.{Employments, TaxSummaryDetails, TotalLiability}
import uk.gov.hmrc.personaltaxsummary.domain.PersonalTaxSummaryContainer
import uk.gov.hmrc.personaltaxsummary.viewmodelfactories.util.TaxSummaryHelper
import uk.gov.hmrc.personaltaxsummary.viewmodels.{Band, BandedGraph, EstimatedIncomeViewModel}
import uk.gov.hmrc.play.views.helpers.MoneyPounds

object EstimatedIncomeViewModelFactory extends ViewModelFactory[EstimatedIncomeViewModel] {
  override def createObject(nino: Nino, container: PersonalTaxSummaryContainer): EstimatedIncomeViewModel = {
    val details = container.details

    val incTax: Boolean = details.increasesTax match {
      case Some(_) => true
      case _ => false
    }

    val estimatedTotalTax: BigDecimal = details.totalLiability.get.totalTax
    val decTaxTotal: BigDecimal = details.decreasesTax.get.total
    val incTotal: BigDecimal = details.increasesTax.get.total
    val reliefs: Boolean = TaxSummaryHelper.getPPR(details)._1 > BigDecimal(0) || TaxSummaryHelper.getGiftAid(details)._1 > BigDecimal(0)
    val emps = details.taxCodeDetails.flatMap(_.employment)
    val listEmps = fetchTaxCodeList(emps)
    val potentialUnderpayment = fetchPotentialUnderpayment(details)

    val totalLiability = details.totalLiability.get
    val additionalTable = createAdditionalTable(totalLiability)
    val additionalTableTotal = MoneyPounds(getTotalAdditionalTaxDue(totalLiability), 2).quantity
    val reductionsTable: List[(String, String, String)] = createReductionsTable(totalLiability, container.links)
    val reductionsTableTotal = "-" + MoneyPounds(getTotalReductions(totalLiability), 2).quantity
    val graph = createBandedGraph(details)
    val dividends = details.increasesTax.flatMap(_.incomes.map(inc => inc.noneTaxCodeIncomes)).flatMap(_.dividends)
    val nextYearTaxTotal = {
      val taxObjects = details.accounts.filter(_.year == TaxYear().next).flatMap(_.nps).map(_.taxObjects)
      taxObjects.flatMap(_.values).flatMap(_.totalTax).sum
    }

    EstimatedIncomeViewModel(
      incTax,
      estimatedTotalTax,
      incTotal,
      decTaxTotal,
      reliefs,
      listEmps,
      potentialUnderpayment,
      additionalTable,
      additionalTableTotal,
      reductionsTable,
      reductionsTableTotal,
      graph,
      TaxSummaryHelper.cyPlusOneAvailable(details),
      dividends,
      None,
      None,
      nextYearTaxTotal
    )
  }

  def mergedBands(taxBands: List[TaxBand]): Option[Band] = {
    val otherThanZeroBand = taxBands.filter(_.rate != 0)

    if (otherThanZeroBand.nonEmpty) {
      Some(Band("Band", calcBarPercentage(otherThanZeroBand.map(_.income).sum, taxBands),
        if (otherThanZeroBand.size > 1) "Check in more detail" else otherThanZeroBand.map(_.rate).head.toString() + "%",
        otherThanZeroBand.map(_.income).sum,
        otherThanZeroBand.map(_.tax).sum,
        if (otherThanZeroBand.size > 1) "TaxedIncome" else otherThanZeroBand.map(_.bandType).head.getOrElse("NA"))
      )
    }
    else {
      None
    }
  }

  def getUpperBand(taxBands: List[TaxBand]): BigDecimal = {
    taxBands match {
      case Nil => BigDecimal(1) //TODO remove this in refactoring
      case bands =>
        val lstBand = bands.last
        val income = taxBands.map(_.income).sum
        val zeroBandSum = taxBands.filter(_.rate == 0).filter(_.bandType == "pa").map(_.income).sum
        val upperBand: BigDecimal = {
          if (lstBand.upperBand.contains(0)) {
            lstBand.lowerBand.map(lBand => lBand + zeroBandSum)
          }
          else {
            lstBand.upperBand.map(upBand => upBand + zeroBandSum)
          }
        }.getOrElse(150000)

        if (income > upperBand) income
        else upperBand
    }
  }

  def calcBarPercentage(incomeBand: BigDecimal, taxBands: List[TaxBand]): BigDecimal = {

    val percentage = (incomeBand * 100) / getUpperBand(taxBands)
    percentage.setScale(2, BigDecimal.RoundingMode.FLOOR)
  }

  def individualBands(taxBands: List[TaxBand]): List[Band] =
    for (taxBand <- taxBands.filter(_.rate == 0)) yield Band("TaxFree", calcBarPercentage(taxBand.income, taxBands),
      "0%", taxBand.income, taxBand.tax, taxBand.bandType.getOrElse("NA"))

  def createBandedGraph(details: TaxSummaryDetails): BandedGraph = {
    val taxbands: List[TaxBand] = retrieveTaxBands(details)
    val zeroRateBands: List[Band] = individualBands(taxbands)
    val otherRateBands: Option[Band] = mergedBands(taxbands)

    val allBands = otherRateBands match {
      case Some(band) => zeroRateBands :+ band
      case _ => zeroRateBands
    }

    BandedGraph("taxGraph",
      allBands,
      0,
      getUpperBand(taxbands),
      allBands.map(_.income).sum,
      zeroRateBands.map(_.barPercentage).sum,
      zeroRateBands.map(_.income).sum,
      allBands.map(_.barPercentage).sum,
      allBands.map(_.tax).sum)
  }

  override def createObject(nino: Nino, details: TaxSummaryDetails): EstimatedIncomeViewModel = {
    createObject(nino, PersonalTaxSummaryContainer(details, Map.empty))
  }

  private def retrieveTaxBands(details: TaxSummaryDetails): List[TaxBand] = {
    val taxObjects = details.currentYearAccounts.flatMap(_.nps).map(_.taxObjects)
    val seqBands = taxObjects.map(_.values.toList).getOrElse(Nil)
    val taxBands = seqBands.flatMap(_.taxBands)
    taxBands.flatten
  }

  private def fetchTaxCodeList(emps: Option[List[Employments]]): List[String] = {
    emps match {
      case Some(list) => for (e <- list) yield {
        e.taxCode.get
      }
      case _ => List[String]()
    }
  }

  private def fetchPotentialUnderpayment(details: TaxSummaryDetails): Boolean = {
    val incomesWithUnderpayment = details.increasesTax.flatMap(_.incomes.map(incomes =>
      TaxSummaryHelper.sortedTaxableIncomes(incomes.taxCodeIncomes).filter(_.tax.potentialUnderpayment.isDefined))).getOrElse(Nil)
    incomesWithUnderpayment.foldLeft(BigDecimal(0))((total, income) =>
      income.tax.potentialUnderpayment.getOrElse(BigDecimal(0)) + total)
    match {
      case x if x > 0 => true
      case _ => false
    }
  }

  private def createAdditionalTable(totalLiability: TotalLiability): List[(String, String)] = {
    val underPayment = fetchTaxTitleAndAmount(totalLiability.underpaymentPreviousYear, "tai.taxCalc.UnderpaymentPreviousYear.title")
    val childBenefitTax = fetchTaxTitleAndAmount(totalLiability.childBenefitTaxDue, "tai.taxCalc.childBenefit.title")
    val outStandingDebt = fetchTaxTitleAndAmount(totalLiability.outstandingDebt, "tai.taxCalc.OutstandingDebt.title")
    val excessGiftAidTax = totalLiability.liabilityAdditions.flatMap(_.excessGiftAidTax.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0))
    val excessGiftAidTaxMessage = fetchTaxTitleAndAmount(excessGiftAidTax, "tai.taxCalc.excessGiftAidTax.title")
    val excessWidowsAndOrphans = totalLiability.liabilityAdditions.flatMap(_.excessWidowsAndOrphans.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0))
    val excessWidowsAndOrphansMessage = fetchTaxTitleAndAmount(excessWidowsAndOrphans, "tai.taxCalc.excessWidowsAndOrphans.title")
    val pensionPaymentsAdjustment = totalLiability.liabilityAdditions.flatMap(_.pensionPaymentsAdjustment.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0))
    val pensionPaymentsAdjustmentMessage = fetchTaxTitleAndAmount(pensionPaymentsAdjustment, "tai.taxCalc.pensionPaymentsAdjustment.title")

    val additionalTable = List(
      underPayment,
      childBenefitTax,
      outStandingDebt,
      excessGiftAidTaxMessage,
      excessWidowsAndOrphansMessage,
      pensionPaymentsAdjustmentMessage
    ).flatten
    additionalTable
  }

  private def fetchTaxTitleAndAmount(amount: BigDecimal, messageKey: String): Option[(String, String)] = {
    if (amount > 0) {
      Some(Messages(messageKey), MoneyPounds(amount, 2).quantity)
    } else {
      None
    }
  }

  private def getTotalAdditionalTaxDue(totalLiability: TotalLiability): BigDecimal = {
    totalLiability.underpaymentPreviousYear +
      totalLiability.childBenefitTaxDue +
      totalLiability.outstandingDebt +
      totalLiability.liabilityAdditions.flatMap(_.excessGiftAidTax.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0)) +
      totalLiability.liabilityAdditions.flatMap(_.excessWidowsAndOrphans.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0)) +
      totalLiability.liabilityAdditions.flatMap(_.pensionPaymentsAdjustment.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0))
  }

  private def createReductionsTable(totalLiability: TotalLiability, links: Map[String, String]): List[(String, String, String)] = {
    val nci = totalLiability.nonCodedIncome.map(_.totalTax.getOrElse(BigDecimal(0))).getOrElse(BigDecimal(0))
    val nonCodedIncome = fetchTaxAmountTitleAndDescription(nci, "tai.taxCollected.atSource.otherIncome.title",
      "tai.taxCollected.atSource.otherIncome.description")
    val ukd = totalLiability.taxCreditOnUKDividends.getOrElse(BigDecimal(0))

    val interestRate = 10
    val basicRate = 20
    val ukDividends = fetchTaxAmountTitleAndDescription(ukd, "tai.taxCollected.atSource.dividends.title",
      "tai.taxCollected.atSource.dividends.taxFreeAmount`description", interestRate)

    val interest = totalLiability.taxOnBankBSInterest.getOrElse(BigDecimal(0))
    val bankInterest = fetchTaxAmountTitleAndDescription(interest, "tai.taxCollected.atSource.bank.title",
      "tai.taxCollected.atSource.bank.description", basicRate)
    val maNet = totalLiability.liabilityReductions.flatMap(_.marriageAllowance.map(_.marriageAllowanceRelief)).getOrElse(BigDecimal(0))
    val maGross = totalLiability.liabilityReductions.flatMap(_.marriageAllowance.map(_.marriageAllowance)).getOrElse(BigDecimal(0))
    val maValue = fetchMarriageAllowanceAmount(maNet, maGross, links.getOrElse("marriageAllowance", ""))
    val mp = totalLiability.liabilityReductions.flatMap(_.maintenancePayments.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0))
    val mpCoding = totalLiability.liabilityReductions.flatMap(_.maintenancePayments.map(_.codingAmount)).getOrElse(BigDecimal(0))
    val maint = fetchMaintenancePayments(mp, mpCoding, links.getOrElse("maintenancePayments", ""))

    val enterpriseInvestmentScheme = totalLiability.liabilityReductions.flatMap(_.enterpriseInvestmentSchemeRelief.map(
      _.amountInTermsOfTax)).getOrElse(BigDecimal(0))

    val enterpriseInvestmentSchemeRelief = fetchReliefAmountAndDescription(enterpriseInvestmentScheme, enterpriseInvestmentScheme,
      "tai.taxCollected.atSource.enterpriseInvestmentSchemeRelief.title", "tai.taxCollected.atSource.enterpriseInvestmentSchemeRelief.description")
    val cr = totalLiability.liabilityReductions.flatMap(_.concessionalRelief.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0))
    val concessionalRelief = fetchReliefAmountAndDescription(enterpriseInvestmentScheme, cr,
      "tai.taxCollected.atSource.concessionalRelief.title", "tai.taxCollected.atSource.concessionalRelief.description")
    val dtr = totalLiability.liabilityReductions.flatMap(_.doubleTaxationRelief.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0))
    val doubleTaxationRelief = fetchReliefAmountAndDescription(enterpriseInvestmentScheme, dtr,
      "tai.taxCollected.atSource.doubleTaxationRelief.title", "tai.taxCollected.atSource.doubleTaxationRelief.description")

    val reductionsTable = List(
      nonCodedIncome,
      ukDividends,
      bankInterest,
      maValue,
      maint,
      enterpriseInvestmentSchemeRelief,
      concessionalRelief,
      doubleTaxationRelief
    ).flatten
    reductionsTable
  }

  private def fetchReliefAmountAndDescription(conditionAmount: BigDecimal, amount: BigDecimal, titleKey: String, descriptionKey: String) = {
    if (conditionAmount > 0) {
      Some(
        Messages(titleKey),
        "-" + MoneyPounds(amount).quantity,
        Messages(descriptionKey)
      )
    } else {
      None
    }
  }

  private def fetchTaxAmountTitleAndDescription(amount: BigDecimal, titleKey: String, descriptionKey: String) = {
    if (amount > 0) {
      Some(Messages(titleKey),
        "-" + MoneyPounds(amount, 2).quantity,
        Messages(descriptionKey)
      )
    } else {
      None
    }
  }

  private def fetchTaxAmountTitleAndDescription(amount: BigDecimal, titleKey: String, descriptionKey: String, descriptionArg: Int) = {
    if (amount > 0) {
      Some(Messages(titleKey),
        "-" + MoneyPounds(amount, 2).quantity,
        Messages(descriptionKey, descriptionArg)
      )
    } else {
      None
    }
  }

  private def fetchMarriageAllowanceAmount(maNet: BigDecimal, maGross: BigDecimal, link: String): Option[(String, String, String)] = {
    if (maNet > 0) {
      Some(
        Messages("tai.taxCollected.atSource.marriageAllowance.title"),
        "-" + MoneyPounds(maNet, 2).quantity,
        Messages(
          "tai.taxCollected.atSource.marriageAllowance.description",
          MoneyPounds(maGross).quantity,
          link
        )
      )
    } else {
      None
    }
  }

  private def fetchMaintenancePayments(mp: BigDecimal, mpCoding: BigDecimal, link: String): Option[(String, String, String)] = {
    if (mp > 0) {
      Some(
        Messages("tai.taxCollected.atSource.maintenancePayments.title"),
        "-" + MoneyPounds(mp).quantity,
        Messages(
          "tai.taxCollected.atSource.maintenancePayments.description",
          MoneyPounds(mpCoding).quantity,
          link
        )
      )
    } else {
      None
    }
  }

  private def getTotalReductions(totalLiability: TotalLiability) = {

    totalLiability.taxOnBankBSInterest.getOrElse(BigDecimal(0)) +
      totalLiability.taxCreditOnUKDividends.getOrElse(BigDecimal(0)) +
      totalLiability.taxCreditOnForeignInterest.getOrElse(BigDecimal(0)) +
      totalLiability.taxCreditOnForeignIncomeDividends.getOrElse(BigDecimal(0)) +
      totalLiability.nonCodedIncome.map(_.totalTax.getOrElse(BigDecimal(0))).getOrElse(BigDecimal(0)) +
      totalLiability.liabilityReductions.flatMap(_.marriageAllowance.map(_.marriageAllowanceRelief)).getOrElse(BigDecimal(0)) +
      totalLiability.liabilityReductions.flatMap(_.enterpriseInvestmentSchemeRelief.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0)) +
      totalLiability.liabilityReductions.flatMap(_.concessionalRelief.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0)) +
      totalLiability.liabilityReductions.flatMap(_.maintenancePayments.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0)) +
      totalLiability.liabilityReductions.flatMap(_.doubleTaxationRelief.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0))
  }

}