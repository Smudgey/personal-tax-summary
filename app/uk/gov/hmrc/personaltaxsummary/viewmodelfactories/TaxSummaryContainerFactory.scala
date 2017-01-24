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

import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.model.TaxSummaryDetails
import uk.gov.hmrc.personaltaxsummary.domain.{EstimatedIncomeWrapper, TaxSummaryContainer}
import uk.gov.hmrc.personaltaxsummary.viewmodelfactories.util.TaxSummaryHelper


object TaxSummaryContainerFactory extends ViewModelFactory[TaxSummaryContainer] {
  override def createObject(nino: Nino, details: TaxSummaryDetails): TaxSummaryContainer = {
    val incomeTax = IncomeTaxViewModelFactory.createObject(nino, details)
    if (!isGateKeepered(details)) {
      val estimatedIncome = EstimatedIncomeViewModelFactory.createObject(nino, details)
      val potentialUnderPayment = getPotentialUnderpayment(details)
      val taxableIncome = YourTaxableIncomeViewModelFactory.createObject(nino, details)
      val wrappedEstimatedIncome = EstimatedIncomeWrapper(estimatedIncome, potentialUnderPayment)
      TaxSummaryContainer(
        details,
        incomeTax,
        Some(wrappedEstimatedIncome),
        Some(taxableIncome),
        None
      )
    } else {
      TaxSummaryContainer(
        details,
        incomeTax,
        None,
        None,
        details.gateKeeper
      )
    }
  }

  def isGateKeepered(taxSummary: TaxSummaryDetails): Boolean = {
    taxSummary.gateKeeper.exists(_.gateKeepered)
  }

  def getPotentialUnderpayment(taxDetails: TaxSummaryDetails): Option[BigDecimal] = {
    val incomesWithUnderpayment = taxDetails.increasesTax
      .flatMap(_.incomes.map(incomes =>
        TaxSummaryHelper.sortedTaxableIncomes(incomes.taxCodeIncomes).filter(_.tax.potentialUnderpayment.isDefined)))
      .getOrElse(Nil)

    incomesWithUnderpayment.foldLeft(BigDecimal(0))((total, income) =>
      income.tax.potentialUnderpayment.getOrElse(BigDecimal(0)) + total)
    match {
      case x if x > 0 => Some(x)
      case _ => None
    }
  }
}