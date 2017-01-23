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

package uk.gov.hmrc.personaltaxsummary.services

import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.model.TaxSummaryDetails
import uk.gov.hmrc.personaltaxsummary.connectors.TaiConnector
import uk.gov.hmrc.personaltaxsummary.domain.{EstimatedIncomeWrapper, TaxSummaryContainer}
import uk.gov.hmrc.personaltaxsummary.viewmodelfactories.util.TaxSummaryHelper
import uk.gov.hmrc.personaltaxsummary.viewmodelfactories.{EstimatedIncomeViewModelFactory, IncomeTaxViewModelFactory, YourTaxableIncomeViewModelFactory}
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait TaiService {
  val taiConnector: TaiConnector

  def getSummary(nino: Nino, year:Int)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Option[TaxSummaryContainer]] = {
    taiConnector.taxSummary(nino, year).map {
      case Some(taxSummaryDetails) =>
        val incomeTax = IncomeTaxViewModelFactory.createObject(nino, taxSummaryDetails)
        if (!isGateKeepered(taxSummaryDetails)) {
          val estimatedIncome = EstimatedIncomeViewModelFactory.createObject(nino, taxSummaryDetails)
          val potentialUnderPayment = getPotentialUnderpayment(taxSummaryDetails)
          val taxableIncome = YourTaxableIncomeViewModelFactory.createObject(nino, taxSummaryDetails)
          val wrappedEstimatedIncome = EstimatedIncomeWrapper(estimatedIncome, potentialUnderPayment)
          Some(
            TaxSummaryContainer(
              taxSummaryDetails,
              incomeTax,
              Some(wrappedEstimatedIncome),
              Some(taxableIncome),
              None
            )
          )
        } else {
          Some(
            TaxSummaryContainer(
              taxSummaryDetails,
              incomeTax,
              None,
              None,
              taxSummaryDetails.gateKeeper
            )
          )
        }
      case _ => None
    }
  }

  def isGateKeepered(taxSummary: TaxSummaryDetails): Boolean = {
    taxSummary.gateKeeper.exists(_.gateKeepered)
  }

  def getPotentialUnderpayment(taxDetails : TaxSummaryDetails): Option[BigDecimal] = {
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