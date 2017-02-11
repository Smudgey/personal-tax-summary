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
import uk.gov.hmrc.model._
import uk.gov.hmrc.model.nps2.IabdType._
import uk.gov.hmrc.personaltaxsummary.domain.{BenefitsDataWrapper, MessageWrapper, PersonalTaxSummaryContainer}
import uk.gov.hmrc.personaltaxsummary.viewmodelfactories.util.TaxSummaryHelper
import uk.gov.hmrc.personaltaxsummary.viewmodels.{EmploymentPension, EstimatedIncomeViewModel, YourTaxableIncomeViewModel}
import uk.gov.hmrc.play.views.helpers.MoneyPounds

object YourTaxableIncomeViewModelFactory extends ViewModelFactory[YourTaxableIncomeViewModel] {

  override def createObject(nino: Nino, details: TaxSummaryDetails): YourTaxableIncomeViewModel = {
    createObject(nino, PersonalTaxSummaryContainer(details, Map.empty))
  }

  override def createObject(nino: Nino, container: PersonalTaxSummaryContainer): YourTaxableIncomeViewModel = {

    val details = container.details

    val increasesTax: Option[IncreasesTax] = details.increasesTax
    val totalLiability: Option[TotalLiability] = details.totalLiability
    val incomeTax: BigDecimal = totalLiability.map(_.totalTax).getOrElse(BigDecimal(0))
    val income: BigDecimal = increasesTax.map(_.total).getOrElse(BigDecimal(0))
    val taxFreeAmount: BigDecimal = details.decreasesTax.map(_.total).getOrElse(BigDecimal(0))
    val taxCodeEmploymentList = details.taxCodeDetails.flatMap(_.employment)
    val taxCodeList = taxCodeEmploymentList match {
      case Some(list) => for (e <- list) yield {
        e.taxCode.get
      }
      case _ => List[String]()
    }

    val incomes = details.increasesTax.flatMap(incTax => incTax.incomes)
    val taxCodeIncomes = incomes.map(inc => inc.taxCodeIncomes)
    val nonTaxCodeIncomes = incomes.map(inc => inc.noneTaxCodeIncomes)
    val investmentIncomeData = YourTaxableIncomeHelper.createInvestmentIncomeTable(nonTaxCodeIncomes)
    val otherIncome = fetchOtherIncome(nonTaxCodeIncomes)

    /* this is a temporary fix until data structure re-write, its purpose is to remove the finance iadbs from the other income table*/
    val financeDivIABDS = List(NationalSavings.code,SavingsBond.code,PurchasedLifeAnnuities.code,UnitTrust.code,StockDividend.code,
      ForeignInterestAndOtherSavings.code,ForeignDividendIncome.code)
    val otherIncomeIabds = otherIncome.iabdSummaries filter {
      iabd => !financeDivIABDS.contains(iabd.iabdType) && iabd.iabdType != 125
    }
    val otherIncomeData = YourTaxableIncomeHelper.createOtherIncomeTable(nonTaxCodeIncomes, otherIncomeIabds)
    val employmentPension: (BigDecimal, Boolean, Boolean) = fetchEmploymentPension(increasesTax)
    val benefitsFromEmployment = fetchEmploymentBenefits(increasesTax)
    val benefitsData = YourTaxableIncomeHelper.createBenefitsTable(benefitsFromEmployment, container.links)
    val taxableBenefitsData = YourTaxableIncomeHelper.createTaxableBenefitTable(nonTaxCodeIncomes, taxCodeIncomes)

    YourTaxableIncomeViewModel(
      taxFreeAmount,
      incomeTax,
      income,
      taxCodeList,
      increasesTax,
      EmploymentPension(taxCodeIncomes = taxCodeIncomes,employmentPension._1,employmentPension._2,employmentPension._3),
      MessageWrapper.applyForList3(investmentIncomeData._1),
      investmentIncomeData._2,
      MessageWrapper.applyForList3(otherIncomeData._1),
      otherIncomeData._2,
      BenefitsDataWrapper.applyBenefit(benefitsData._1),
      benefitsData._2,
      MessageWrapper.applyForList3(taxableBenefitsData._1),
      taxableBenefitsData._2,
      TaxSummaryHelper.cyPlusOneAvailable(details)
    )
  }

  private def fetchOtherIncome(nonTaxCodeIncomes: Option[NoneTaxCodeIncomes]): TaxComponent = {
    val oIncome = nonTaxCodeIncomes.flatMap(_.otherIncome)
    if (oIncome.isDefined) {
      oIncome.get
    } else {
      TaxComponent(BigDecimal(0), 0, "", List())
    }
  }

  private def fetchEmploymentBenefits(increasesTax: Option[IncreasesTax]): TaxComponent = {
    increasesTax.map { incTax =>
      if (incTax.benefitsFromEmployment.isDefined) {
        incTax.benefitsFromEmployment.get
      } else {
        TaxComponent(BigDecimal(0), 0, "", List())
      }
    }.getOrElse(TaxComponent(BigDecimal(0), 0, "", List()))
  }

  private def fetchEmploymentPension(increasesTax: Option[IncreasesTax]): (BigDecimal, Boolean, Boolean) = {
    increasesTax.map { increasesTax =>
      fetchEmploymentPensions(increasesTax)
    }.getOrElse(BigDecimal(0), false, false)
  }

  private def fetchEmploymentPensions(increasesTax: IncreasesTax): (BigDecimal, Boolean, Boolean) = {
    val taxCodeIncomes = increasesTax.incomes.map(_.taxCodeIncomes)
    val employmentIncomes = taxCodeIncomes.flatMap(_.employments)
    val empIncome = employmentIncomes.map(_.taxCodeIncomes).getOrElse(List())
    val occupationalPensionIncomes = taxCodeIncomes.flatMap(_.occupationalPensions)
    val occPensionIncome = occupationalPensionIncomes.map(_.taxCodeIncomes).getOrElse(List())
    val ceasedIncomes = taxCodeIncomes.flatMap(_.ceasedEmployments)
    val ceasedIncome = ceasedIncomes.map(_.taxCodeIncomes).getOrElse(List())

    val empIncomeAmount = for {
      payeIncome <- empIncome
      amount = payeIncome.income.getOrElse(BigDecimal(0))
    } yield amount
    val empAmt = empIncomeAmount.sum

    val occPensionIncomeAmount = for {
      payeIncome <- occPensionIncome
      amount = payeIncome.income.getOrElse(BigDecimal(0))
    } yield amount
    val occPensionAmt = occPensionIncomeAmount.sum

    val ceasedIncomeAmount = for {
      payeIncome <- ceasedIncome

      amount = payeIncome.income.getOrElse(BigDecimal(0))
    } yield amount
    val ceasedAmt = ceasedIncomeAmount.sum

    val hasOccPension = if (occPensionAmt > 0) true else false
    val hasEmployment = if (ceasedAmt > 0 || empAmt > 0) true else false
    val totalEmploymentPensionAmt = empAmt + occPensionAmt + ceasedAmt

    (totalEmploymentPensionAmt, hasEmployment, hasOccPension)
  }
}

object YourTaxableIncomeHelper {
  def extractFromTaxCodeIncomes(taxComponent: Option[TaxComponent]): TaxComponent = {
    if (taxComponent.isDefined) {
      taxComponent.get
    } else {
      TaxComponent(BigDecimal(0), 0, "", List())
    }
  }

  def iterateTaxComponent(iabdSummaryList:List[IabdSummary]): List[Option[(String, String, String)]] = {
    for {
      iabdSummary <- iabdSummaryList

      message: (String, String) = {
        if (!Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}").isEmpty) {
          (Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"),
            Messages(s"tai.iabdSummary.type-${iabdSummary.iabdType}"))
        }
        else {
          (Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"), "")
        }

      }

      rawAmt = iabdSummary.amount
      amount = MoneyPounds(rawAmt, 0).quantity

      taxComponentData = if (rawAmt > 0) {
        Some(message._2, amount, message._1)
      } else {
        None
      }

    } yield taxComponentData
  }

  def createInvestmentIncomeTable(nonTaxCodeIncomes: Option[NoneTaxCodeIncomes]): (List[(String, String, String)], BigDecimal) = {

    val dividends = extractFromTaxCodeIncomes(nonTaxCodeIncomes.flatMap(_.dividends))

    val bankInterest = extractFromTaxCodeIncomes(nonTaxCodeIncomes.flatMap(_.bankBsInterest))

    val unTaxedBankInterest = extractFromTaxCodeIncomes(nonTaxCodeIncomes.flatMap(_.untaxedInterest))

    val foreignInterest = extractFromTaxCodeIncomes(nonTaxCodeIncomes.flatMap(_.foreignInterest))

    val foreignDividends = extractFromTaxCodeIncomes(nonTaxCodeIncomes.flatMap(_.foreignDividends))


    val totalInvestmentIncome: BigDecimal = dividends.amount +
      bankInterest.amount +
      unTaxedBankInterest.amount +
      foreignInterest.amount +
      foreignDividends.amount

    val dividendsRows: List[Option[(String, String, String)]] =
      iterateTaxComponent(dividends.iabdSummaries)

    val bankInterestRows: List[Option[(String, String, String)]] =
      iterateTaxComponent(bankInterest.iabdSummaries)

    val unTaxedInterestRows: List[Option[(String, String, String)]] =
      iterateTaxComponent(unTaxedBankInterest.iabdSummaries)

    val foreignInterestRows: List[Option[(String, String, String)]] =
      iterateTaxComponent(foreignInterest.iabdSummaries)

    val foreignDividendsRows: List[Option[(String, String, String)]] =
      iterateTaxComponent(foreignDividends.iabdSummaries)


    val allInvestmentIncomeRows = dividendsRows ::: bankInterestRows :::
      unTaxedInterestRows ::: foreignInterestRows ::: foreignDividendsRows
    (allInvestmentIncomeRows.flatten, totalInvestmentIncome)
  }

  //temp fix to move Bereavement allowance into taxable state benefit table remove/refactor after data-structure change
  def createTaxableBenefitTable(nonTaxCodeIncomes: Option[NoneTaxCodeIncomes],
                                taxCodeIncomes: Option[TaxCodeIncomes],
                                bevAllowance: Option[IabdSummary] = None): (List[(String, String, String)], BigDecimal) = {

    val taxableStateBenefit = extractFromTaxCodeIncomes(nonTaxCodeIncomes.flatMap(_.taxableStateBenefit))
    val taxableStateBenefitIncomes = taxCodeIncomes.flatMap(_.taxableStateBenefitIncomes)
    val taxCodeIncome = taxableStateBenefitIncomes.map(_.taxCodeIncomes).getOrElse(List())
    val taxableStateBenefitEmploymentRows: List[(Option[(String, String, String)], scala.BigDecimal)] =
      for {
        payeIncome <- taxCodeIncome
        message: (String) = payeIncome.name
        rawAmt = payeIncome.income.getOrElse(BigDecimal(0))
        amount = MoneyPounds(rawAmt, 0).quantity
        taxableStateBenefitData = if (rawAmt > 0) Some(message, amount, "") else None
      } yield (taxableStateBenefitData, rawAmt)

    val unzipTaxableStateBenEmpRows = taxableStateBenefitEmploymentRows.unzip
    val statePension = nonTaxCodeIncomes.flatMap(_.statePension).getOrElse(BigDecimal(0))
    val statePensionLumpSum = nonTaxCodeIncomes.flatMap(_.statePensionLumpSum).getOrElse(BigDecimal(0))
    val statePensionData = if (statePension > 0) {
      Some(Messages("tai.income.statePension.title"),
        MoneyPounds(statePension, 0).quantity,
        Messages("tai.iabdSummary.description-state-pension"))
    } else { None }

    val statePensionLumpSumData = if (statePensionLumpSum > 0) {
      Some(Messages("tai.income.statePensionLumpSum.total"),
        MoneyPounds(statePensionLumpSum, 0).quantity, "")
    } else { None }

    val tStateBenefitRows: List[Option[(String, String, String)]] = iterateTaxComponent(taxableStateBenefit.iabdSummaries)
    val totalBenefits: BigDecimal = statePension +
      statePensionLumpSum + taxableStateBenefit.amount +
      unzipTaxableStateBenEmpRows._2.sum + bevAllowance.map(_.amount).getOrElse(0)

    //need to put bev allowance description
    val bevAllowanceRow = if (bevAllowance.isDefined) {
      Some(Messages("tai.iabdSummary.type-125"),
        MoneyPounds(bevAllowance.map(_.amount).getOrElse(0), 0).quantity,
        Messages(""))
    } else { None }

    val statePensionRows = List(statePensionData, statePensionLumpSumData, bevAllowanceRow)
    val allOtherIncomeRows = tStateBenefitRows ::: unzipTaxableStateBenEmpRows._1 ::: statePensionRows
    (allOtherIncomeRows.flatten, totalBenefits)
  }

  def createOtherIncomeTable(nonTaxCodeIncomes: Option[NoneTaxCodeIncomes], otherIncome: List[IabdSummary]): (List[(String, String, String)], BigDecimal) = {

    val otherIncomeRows: List[Option[(String, String, String)]] =
      iterateTaxComponent(otherIncome)


    val otherPension = extractFromTaxCodeIncomes(nonTaxCodeIncomes.flatMap(_.otherPensions))

    val otherPensionRows =
      iterateTaxComponent(otherPension.iabdSummaries)

    val totalOtherIncomeAmount = otherIncome.map(_.amount).sum + otherPension.amount

    val allOtherIncomeRows = otherIncomeRows ::: otherPensionRows

    (allOtherIncomeRows.flatten, totalOtherIncomeAmount)

  }

  def createBenefitsTable(benefitsFromEmployment: TaxComponent, links: Map[String, String]): (List[(String, String, String, String, Option[Int], Option[Int])], BigDecimal) = {
    val benefitsRows: List[Option[(String, String, String, String, Option[Int], Option[Int])]] =
      for {iabdSummary <- benefitsFromEmployment.iabdSummaries

           message: (String, String, String) = iabdSummary.iabdType match {
             case MedicalInsurance.code =>
               (Messages(s"tai.iabdSummary.employmentBenefit.type-${iabdSummary.iabdType}",
                 iabdSummary.employmentName.getOrElse("")), Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"),
                 links.getOrElse("medBenefitServiceUrl", ""))

             case CarBenefit.code =>
               (Messages(s"tai.iabdSummary.employmentBenefit.type-${iabdSummary.iabdType}",
                 iabdSummary.employmentName.getOrElse("")), Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"),
                 links.getOrElse("companyCarServiceUrl", ""))

             case _ =>
               (Messages(s"tai.iabdSummary.employmentBenefit.type-${iabdSummary.iabdType}",
                 iabdSummary.employmentName.getOrElse("")),
                 Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"),
                 "")
           }

           rawAmt = iabdSummary.amount
           amount = MoneyPounds(rawAmt, 0).quantity

           benefitsData = if (rawAmt > 0) {
             Some(message._1, amount, message._2, message._3, iabdSummary.employmentId, Some(iabdSummary.iabdType))
           } else {
             None
           }

      } yield benefitsData

    val totalBenefitsAmount = benefitsFromEmployment.amount
    (benefitsRows.flatten, totalBenefitsAmount)
  }
}
