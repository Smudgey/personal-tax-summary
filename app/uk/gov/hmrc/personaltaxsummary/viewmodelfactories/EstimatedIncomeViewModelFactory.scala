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

package uk.gov.hmrc.personaltaxsummary.viewmodelfactories

import play.api.Play.current
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.model._
import uk.gov.hmrc.model.nps2.{TaxBand, TaxObject}
import uk.gov.hmrc.model.tai.TaxYear
import uk.gov.hmrc.personaltaxsummary.config.FeatureTogglesConfig
import uk.gov.hmrc.personaltaxsummary.domain.PersonalTaxSummaryContainer
import uk.gov.hmrc.personaltaxsummary.viewmodelfactories.util.TaiConstants.higherRateBandIncome
import uk.gov.hmrc.personaltaxsummary.viewmodelfactories.util.{TaxRegion, TaxSummaryHelper}
import uk.gov.hmrc.personaltaxsummary.viewmodels.{AdditionalTaxRow, Band, BandedGraph, EstimatedIncomeViewModel}
import uk.gov.hmrc.play.views.helpers.MoneyPounds

import scala.math.BigDecimal

object EstimatedIncomeViewModelFactory extends ViewModelFactory[EstimatedIncomeViewModel] with TaxRegion with FeatureTogglesConfig {


  override def createObject(nino: Nino, container: PersonalTaxSummaryContainer): EstimatedIncomeViewModel = {
    val details = container.details

    val incTax: Boolean = details.increasesTax match {
      case Some(_) => true
      case _ => false
    }

    val estimatedTotalTax: BigDecimal = details.totalLiability.get.totalTax
    val decTaxTotal: BigDecimal = details.decreasesTax.map(_.total).getOrElse(BigDecimal("0"))
    val incTotal: BigDecimal = details.increasesTax.get.total
    val reliefs: Boolean = TaxSummaryHelper.getPPR(details)._1 > BigDecimal(0) || TaxSummaryHelper.getGiftAid(details)._1 > BigDecimal(0)
    val emps = details.taxCodeDetails.flatMap(_.employment)
    val listEmps = fetchTaxCodeList(emps)
    val potentialUnderpayment = fetchPotentialUnderpayment(details)

    val totalLiability = details.totalLiability.get
    val additionalTable = createAdditionalTable(totalLiability)
    val additionalTableV2 = createAdditionalTableV2(totalLiability,container.links)
    val additionalTableTotal = MoneyPounds(getTotalAdditionalTaxDue(totalLiability), 2).quantity
    val reductionsTable: List[(String, String, String)] = createReductionsTable(totalLiability, container.links)
    val reductionsTableTotal = "-" + MoneyPounds(getTotalReductions(totalLiability), 2).quantity
    val taxBands = retrieveTaxBands(details)
    val personalAllowance = details.decreasesTax.flatMap(_.personalAllowance)
    val oldGraph = createBandedGraphWithBandsOnly(taxBands)
    val newGraph = createBandedGraph(taxBands, personalAllowance, container.links)
    val dividends = details.increasesTax.flatMap(_.incomes.map(inc => inc.noneTaxCodeIncomes)).flatMap(_.dividends)
    val dividendBands = {
      val ukDividendBands = details.currentYearAccounts.flatMap(_.nps).flatMap(_.taxObjects.get(TaxObject.Type.UkDividends))
        ukDividendBands.flatMap(_.taxBands)
    }
    val nextYearTaxTotal = {
      val taxObjects = details.accounts.filter(_.year == TaxYear().next).flatMap(_.nps).map(_.taxObjects)
      taxObjects.flatMap(_.values).flatMap(_.totalTax).sum
    }
    val taxBandTypes = taxBands.flatMap(_.bandType)

    val taxRegion = findTaxRegion(details.taxCodeDetails, scottishTaxRateEnabled)

    EstimatedIncomeViewModel(
      incTax,
      estimatedTotalTax,
      incTotal,
      decTaxTotal,
      reliefs,
      listEmps,
      potentialUnderpayment,
      additionalTable,
      additionalTableV2,
      additionalTableTotal,
      reductionsTable,
      reductionsTableTotal,
      oldGraph,
      TaxSummaryHelper.cyPlusOneAvailable(details),
      dividends,
      dividendBands.map(_.toList),
      fetchTaxReducedZeroMsg(reductionsTable.size, totalLiability.totalTax),
      nextYearTaxTotal,
      taxBandTypes.contains("PSR"),
      taxBandTypes.contains("SR"),
      newGraph,
      taxRegion
    )
  }

  def findTaxRegion(taxCodes: Option[TaxCodeDetails], scottishTaxRateEnabled: Boolean): String = {

    val bandType = for {
      taxCodeDetails <- taxCodes if scottishTaxRateEnabled
      employments <- taxCodeDetails.employment
    } yield {
      if(employments.exists(_.taxCode.getOrElse("").startsWith("S"))) ScottishTaxRegion
      else UkTaxRegion
    }

    bandType.getOrElse(UkTaxRegion)
  }

  def createBandedGraphWithBandsOnly(taxBands: List[TaxBand]): BandedGraph = {
    val bands = taxBands.map(
      taxBand =>
        Band(
          colour = "",
          tablePercentage = taxBand.rate.toString(),
          income = taxBand.income, tax = taxBand.tax,
          bandType = taxBand.bandType.getOrElse(Messages("tai.not-applicable")))
    )
    BandedGraph(id = "taxGraph", bands = bands, incomeTotal = bands.map(_.income).sum, taxTotal = bands.map(_.tax).sum)
  }

  private def fetchTaxReducedZeroMsg(reductions: Int, totalTax: BigDecimal): Option[String] = {
    if(reductions > 0 && totalTax <= 0) {
      Some(Messages("tai.estimatedIncome.reductionsTax.incomeTaxReducedToZeroMessage"))
    } else {
      None
    }
  }

  def mergedBands(taxBands: List[TaxBand], personalAllowance: Option[BigDecimal] = None, links: Map[String, String] = Map.empty): Option[Band] = {
    val nonZeroBands = taxBands.filter(_.rate != 0)

    Option(nonZeroBands.nonEmpty).collect {
      case true =>
        val (tablePercentage, bandType, incomeSum) = getBandValues(nonZeroBands, links)

        Band("Band", calcBarPercentage(incomeSum, taxBands, personalAllowance),
          tablePercentage = tablePercentage,
          income = incomeSum,
          tax = nonZeroBands.map(_.tax).sum,
          bandType = bandType
        )
    }
  }

  private def getBandValues(nonZeroBands: List[TaxBand], links: Map[String, String] = Map.empty) = {
    if (nonZeroBands.size > 1) {
      (links.getOrElse("taxExplanationScreen",""), Messages("tai.taxedIncome.desc"), nonZeroBands.map(_.income).sum)
    } else {
      nonZeroBands.map(otherBand => (otherBand.rate.toString() + "%", otherBand.bandType.getOrElse(Messages("tai.not-applicable")), otherBand.income)).head
    }
  }

  //List should be sorted by rate
  def getUpperBand(taxBands: List[TaxBand], personalAllowance: Option[BigDecimal] = None): BigDecimal = {
    taxBands match  {
      case Nil => BigDecimal(0)
      case _ =>
        val lstBand = taxBands.last
        val income = taxBands.map(_.income).sum
        val taxFreeAllowanceBandSum = taxBands.filter(taxBand => taxBand.rate == 0 && taxBand.bandType.contains("pa")).map(_.income).sum
        val upperBand: BigDecimal = {
          if (lstBand.upperBand.contains(0)) {
            lstBand.lowerBand.map(lBand => lBand + taxFreeAllowanceBandSum)
          } else {
            lstBand.upperBand.map(upBand => {
              if(upBand >= higherRateBandIncome) upBand + taxFreeAllowanceBandSum - personalAllowance.getOrElse(0)
              else upBand + taxFreeAllowanceBandSum
            })
          }
        }.getOrElse(taxFreeAllowanceBandSum)

        if (income > upperBand) income
        else upperBand
    }

  }

  def calcBarPercentage(incomeBand: BigDecimal, taxBands: List[TaxBand], personalAllowance: Option[BigDecimal] = None): BigDecimal = {
    taxBands match {
      case Nil => BigDecimal(0)
      case _ =>
        val percentage = (incomeBand * 100) / getUpperBand(taxBands, personalAllowance)
        percentage.setScale(2, BigDecimal.RoundingMode.FLOOR)
    }
  }

  def individualBands(taxBands: List[TaxBand], personalAllowance: Option[BigDecimal] = None): List[Band] =
    for (taxBand <- taxBands.filter(_.rate == 0)) yield Band("TaxFree", calcBarPercentage(taxBand.income, taxBands, personalAllowance),
      Messages("tai.zero-percentage"), taxBand.income, taxBand.tax, taxBand.bandType.getOrElse(Messages("tai.not-applicable")))

  def individualOtherRateBands(taxBands: List[TaxBand], personalAllowance: Option[BigDecimal] = None): List[Band] =
    for (taxBand <- taxBands) yield Band("Band", calcBarPercentage(taxBand.income, taxBands, personalAllowance),
      Messages("tai.taxRate-percentage", taxBand.rate), taxBand.income, taxBand.tax, taxBand.bandType.getOrElse(Messages("tai.not-applicable")))

  def createBandedGraph(taxBands: List[TaxBand], personalAllowance: Option[BigDecimal] = None, links: Map[String, String] = Map.empty): BandedGraph = {
    taxBands match {
      case Nil => BandedGraph("taxGraph") //This case will never occur
      case taxbands => createGraph(taxbands, personalAllowance, links)
    }
  }

  private def createGraph(taxbands: List[TaxBand], personalAllowance: Option[BigDecimal] = None, links: Map[String, String] = Map.empty): BandedGraph = {
    val (individualBand: List[Band], mergedBand: Option[Band])  = {
      if(taxbands.exists(_.rate == 0))
        (individualBands(taxbands, personalAllowance), mergedBands(taxbands, personalAllowance, links))
      else (individualOtherRateBands(taxbands), None)
    }

    val allBands = mergedBand match {
      case Some(band) => individualBand :+ band
      case _ => individualBand
    }

    val nextHigherBand = getUpperBand(taxbands, personalAllowance)
    val incomeTotal = allBands.map(_.income).sum
    val nextBandMessage = createNextBandMessage(nextHigherBand - incomeTotal)
    val zeroRateBands = individualBand.filter(_.tax == BigDecimal(0))

    BandedGraph("taxGraph",
      allBands,
      nextBand = nextHigherBand,
      incomeTotal = incomeTotal,
      zeroIncomeAsPercentage = zeroRateBands.map(_.barPercentage).sum,
      zeroIncomeTotal = zeroRateBands.map(_.income).sum,
      incomeAsPercentage = allBands.map(_.barPercentage).sum,
      taxTotal = allBands.map(_.tax).sum,
      nextBandMessage = nextBandMessage
    )
  }


  private def createNextBandMessage(amount: BigDecimal): Option[String] = {
    // if amount > 0 then message else None
    Option(amount > 0).collect {
      case true => Messages("tai.taxCalc.nextTaxBand", MoneyPounds(amount, 0).quantity)
    }
  }

  override def createObject(nino: Nino, details: TaxSummaryDetails): EstimatedIncomeViewModel = {
    createObject(nino, PersonalTaxSummaryContainer(details, Map.empty))
  }

  private val mergeAllowanceTaxBands = (taxBands: List[TaxBand], bandType:  String) => {
    val (bands, remBands) = taxBands.partition(_.bandType.contains(bandType))
    bands match {
      case Nil => remBands
      case _ => TaxBand(bands.map(_.bandType).head,
        bands.map(_.code).head,
        bands.map(_.income).sum,
        bands.map(_.tax).sum,
        bands.map(_.lowerBand).head,
        bands.map(_.upperBand).head,
        bands.map(_.rate).head) :: remBands
    }
  }

  def retrieveTaxBands(details: TaxSummaryDetails): List[TaxBand] = {
    val taxObjects = details.currentYearAccounts.flatMap(_.nps).map(_.taxObjects)
    val seqBands = taxObjects.map(_.values.toList).getOrElse(Nil)
    val taxBands = seqBands.flatMap(_.taxBands).flatten
    val mergedPsaBands = mergeAllowanceTaxBands(taxBands, "PSR")
    val mergedSrBands = mergeAllowanceTaxBands(mergedPsaBands, "SR")
    val bands = mergeAllowanceTaxBands(mergedSrBands, "pa")
    bands.sortBy(_.rate)
  }

  private def fetchTaxCodeList(emps: Option[List[Employments]]): List[String] = {
    emps match {
      case Some(list) => for (e <- list) yield {
        e.taxCode.get
      }
      case _ => List[String]()
    }
  }

  def fetchPotentialUnderpayment(details: TaxSummaryDetails): Boolean = {
    val iyaIntoCYPlusOne = containsPositiveValue(details){ tax => tax.inYearAdjustmentIntoCYPlusOne }
    val iyaIntoCY = containsPositiveValue(details){ tax => tax.inYearAdjustmentIntoCY }

    (iyaIntoCY, iyaIntoCYPlusOne) match {
      case (false,true) => true
      case _ => false
    }
  }

  def containsPositiveValue(details: TaxSummaryDetails)(extract: Tax => Option[BigDecimal]):Boolean ={
    val incomesWithUnderpayment = details.increasesTax.flatMap(_.incomes.map(incomes =>
      TaxSummaryHelper.sortedTaxableIncomes(incomes.taxCodeIncomes).filter(income => extract(income.tax).isDefined))).getOrElse(Nil)

    incomesWithUnderpayment.foldLeft(false)((result, income) =>
      extract(income.tax).getOrElse(BigDecimal(0)) > 0 || result)
  }


  private def createAdditionalTable(totalLiability: TotalLiability): List[(String, String)] = {
    additionalTableFactory(totalLiability)(fetchTaxTitleAndAmount)
  }
  private def createAdditionalTableV2(totalLiability: TotalLiability, containerLinks:  Map[String, String]): List[AdditionalTaxRow] = {
    additionalTableFactory(totalLiability)(createAdditionalTaxRow(containerLinks))
  }

  private def additionalTableFactory[R](totalLiability: TotalLiability)(fetchTableRow: (BigDecimal,String, Option[String]) => Option[R]): List[R] ={
    val underPayment = fetchTableRow(totalLiability.underpaymentPreviousYear, "tai.taxCalc.UnderpaymentPreviousYear.title",None)
    val inYearAdjustment = fetchTableRow(totalLiability.inYearAdjustment.fold(BigDecimal(0))(iya => iya), "tai.taxcode.deduction.type-45", Some("underpaymentEstimatePageUrl"))
    val childBenefitTax = fetchTableRow(totalLiability.childBenefitTaxDue, "tai.taxCalc.childBenefit.title",None)
    val outStandingDebt = fetchTableRow(totalLiability.outstandingDebt, "tai.taxCalc.OutstandingDebt.title",None)
    val excessGiftAidTax = totalLiability.liabilityAdditions.flatMap(_.excessGiftAidTax.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0))
    val excessGiftAidTaxMessage = fetchTableRow(excessGiftAidTax, "tai.taxCalc.excessGiftAidTax.title",None)
    val excessWidowsAndOrphans = totalLiability.liabilityAdditions.flatMap(_.excessWidowsAndOrphans.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0))
    val excessWidowsAndOrphansMessage = fetchTableRow(excessWidowsAndOrphans, "tai.taxCalc.excessWidowsAndOrphans.title",None)
    val pensionPaymentsAdjustment = totalLiability.liabilityAdditions.flatMap(_.pensionPaymentsAdjustment.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0))
    val pensionPaymentsAdjustmentMessage = fetchTableRow(pensionPaymentsAdjustment, "tai.taxCalc.pensionPaymentsAdjustment.title",None)

    val additionalTable = List(
      underPayment,
      inYearAdjustment,
      childBenefitTax,
      outStandingDebt,
      excessGiftAidTaxMessage,
      excessWidowsAndOrphansMessage,
      pensionPaymentsAdjustmentMessage
    ).flatten
    additionalTable
  }

  private def fetchTaxTitleAndAmount(amount: BigDecimal, messageKey: String, urlKey: Option[String]=None): Option[(String, String)] = {
    if (amount > 0) {
      Some(Messages(messageKey), MoneyPounds(amount, 2).quantity)
    } else {
      None
    }
  }

  private def createAdditionalTaxRow(links:Map[String,String])(amount: BigDecimal, messageKey: String, urlKey: Option[String]=None): Option[AdditionalTaxRow] = {
    fetchTaxTitleAndAmount(amount,messageKey) match {
      case Some((description,amount)) =>  Some(AdditionalTaxRow(description,amount,links.get(urlKey.getOrElse(""))))
      case _ => None
    }
  }

  private def getTotalAdditionalTaxDue(totalLiability: TotalLiability): BigDecimal = {
      totalLiability.underpaymentPreviousYear +
      totalLiability.inYearAdjustment.fold(BigDecimal(0))(iya => iya) +
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
      "tai.taxCollected.atSource.dividends.description", interestRate)

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