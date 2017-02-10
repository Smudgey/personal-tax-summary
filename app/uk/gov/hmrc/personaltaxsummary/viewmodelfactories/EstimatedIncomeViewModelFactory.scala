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
import uk.gov.hmrc.model.{DecreasesTax, Employments, TaxSummaryDetails, TotalLiability}
import uk.gov.hmrc.personaltaxsummary.domain.{MessageWrapper, PersonalTaxSummaryContainer}
import uk.gov.hmrc.personaltaxsummary.viewmodelfactories.util.{TaxDecorator, TaxSummaryHelper}
import uk.gov.hmrc.personaltaxsummary.viewmodels.{Band, BandedGraph, EstimatedIncomeViewModel}
import uk.gov.hmrc.play.views.helpers.MoneyPounds

object EstimatedIncomeViewModelFactory extends ViewModelFactory[EstimatedIncomeViewModel] {
  override def createObject(nino: Nino, container: PersonalTaxSummaryContainer): EstimatedIncomeViewModel = {
    println(container.links)
    createObject(nino, container.details)
  }

  override def createObject(nino: Nino, details: TaxSummaryDetails): EstimatedIncomeViewModel = {

    val incTax:Boolean = details.increasesTax match {
      case Some(_) => true
      case _ => false
    }

    val estimatedTotalTax:BigDecimal = details.totalLiability.get.totalTax
    val decTaxTotal:BigDecimal = details.decreasesTax.get.total
    val incTotal:BigDecimal = details.increasesTax.get.total
    val reliefs:Boolean = TaxSummaryHelper.getPPR(details)._1 > BigDecimal(0) || TaxSummaryHelper.getGiftAid(details)._1 > BigDecimal(0)
    val emps = details.taxCodeDetails.flatMap(_.employment)
    val listEmps = fetchTaxCodeList(emps)
    val potentialUnderpayment = fetchPotentialUnderpayment(details)

    val totalLiability = details.totalLiability.get
    val additionalTable = createAdditionalTable(totalLiability)
    val additionalTableTotal = MoneyPounds(getTotalAdditionalTaxDue(totalLiability),2).quantity
    val reductionsTable: List[(String, String, String)] = createReductionsTable(totalLiability)
    val reductionsTableTotal = "-" + MoneyPounds(getTotalReductions(totalLiability),2).quantity
    val mergedIncome = totalLiability.mergedIncomes.getOrElse(throw new RuntimeException("No data"))
    val td = TaxDecorator(mergedIncome, paAmount = getActualPA(details.decreasesTax))
    val bands: List[Band] = createTaxBands(td)

    val incomeAsPerc = td.incomeAsPercentage
    val graph = BandedGraph("taxGraph",bands,td.taxBandLabelFirstAmount,td.taxBandLabelLastAmount,td.totalIncome,incomeAsPerc,td.taxBandDecorator.totalTax)
    val dividends = details.increasesTax.flatMap(_.incomes.map(inc => inc.noneTaxCodeIncomes)).flatMap(_.dividends)
    val taxBands = totalLiability.ukDividends.flatMap(_.taxBands)
    val incomeTaxReducedToZeroMessage = fetchIncomeTaxDueAmount(totalLiability, td)

    EstimatedIncomeViewModel(
      incTax,
      estimatedTotalTax,
      incTotal,
      decTaxTotal,
      reliefs,
      listEmps,
      potentialUnderpayment,
      MessageWrapper.applyForList2(additionalTable),
      additionalTableTotal,
      MessageWrapper.applyForList3(reductionsTable),
      reductionsTableTotal,
      graph,
      TaxSummaryHelper.cyPlusOneAvailable(details),
      dividends,
      taxBands,
      incomeTaxReducedToZeroMessage
    )
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

  private def fetchIncomeTaxDueAmount(totalLiability: TotalLiability, td: TaxDecorator): Option[String] = {
    val rawIncomeTaxDue = td.taxBandDecorator.totalTax -
      getTotalReductions(totalLiability)

    if (rawIncomeTaxDue <= 0) {
      Some(Messages("tai.estimatedIncome.reductionsTax.incomeTaxReducedToZeroMessage"))
    } else {
      None
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

  private def createTaxBands(td: TaxDecorator): List[Band] = {
    val bands = for {
      tb <- td.taxBandDescriptions
    } yield Band(
      tb.className,
      tb.widthAsPerc,
      removeDecimalsToString(tb.taxBand.rate.getOrElse(BigDecimal(0))),
      tb.taxBand.income.getOrElse(BigDecimal(0)),
      tb.taxBand.tax.getOrElse(BigDecimal(0))
    )
    bands
  }

  private def removeDecimalsToString(decimal: math.BigDecimal): String = {
    decimal.bigDecimal.stripTrailingZeros.toPlainString
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

  private def createReductionsTable(totalLiability: TotalLiability): List[(String, String, String)] = {
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
    val maValue = fetchMarriageAllowanceAmount(maNet, maGross)
    val mp = totalLiability.liabilityReductions.flatMap(_.maintenancePayments.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0))
    val mpCoding = totalLiability.liabilityReductions.flatMap(_.maintenancePayments.map(_.codingAmount)).getOrElse(BigDecimal(0))
    val maint = fetchMaintenancePayments(mp, mpCoding)

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

  // TODO: check desired Link behaviour
  private def fetchMarriageAllowanceAmount(maNet: BigDecimal, maGross: BigDecimal): Option[(String, String, String)] = {
    if (maNet > 0) {
      Some(
        Messages("tai.taxCollected.atSource.marriageAllowance.title"),
        "-" + MoneyPounds(maNet, 2).quantity,
        Messages(
          "tai.taxCollected.atSource.marriageAllowance.description",
          MoneyPounds(maGross).quantity,
          "<!-- TODO -->"
        )
      )
    } else {
      None
    }
  }

  // TODO: check desired Link behaviour
  private def fetchMaintenancePayments(mp: BigDecimal, mpCoding: BigDecimal): Option[(String, String, String)] = {
    if (mp > 0) {
      Some(
        Messages("tai.taxCollected.atSource.maintenancePayments.title"),
        "-" + MoneyPounds(mp).quantity,
        Messages(
          "tai.taxCollected.atSource.maintenancePayments.description",
          MoneyPounds(mpCoding).quantity,
          "<!-- TODO -->"
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

  private def getActualPA(decreasesTax: Option[DecreasesTax]): BigDecimal = {
    decreasesTax.map(_.personalAllowance.getOrElse(BigDecimal(0))).getOrElse(BigDecimal(0))
  }

}
