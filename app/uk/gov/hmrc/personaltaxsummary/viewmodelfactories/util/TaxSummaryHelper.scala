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

package uk.gov.hmrc.personaltaxsummary.viewmodelfactories.util

import uk.gov.hmrc.model.{TaxCodeIncomes, TaxSummaryDetails}

object TaxSummaryHelper {

  def getPPR(taxSummaryDetails: TaxSummaryDetails): (BigDecimal, BigDecimal) = {

    val pprSource = taxSummaryDetails.extensionReliefs.flatMap(extensionRelief =>
      extensionRelief.personalPension.map(_.sourceAmount)
    ).getOrElse(BigDecimal(0))

    val pprRelief = taxSummaryDetails.extensionReliefs.flatMap(extensionRelief =>
      extensionRelief.personalPension.map(_.reliefAmount)
    ).getOrElse(BigDecimal(0))

    (pprSource, pprRelief)
  }

  def getGiftAid(taxSummaryDetails: TaxSummaryDetails): (BigDecimal, BigDecimal) = {

    val giftAidSource = taxSummaryDetails.extensionReliefs.flatMap(extensionRelief =>
      extensionRelief.giftAid.map(_.sourceAmount)
    ).getOrElse(BigDecimal(0))

    val giftAidRelief = taxSummaryDetails.extensionReliefs.flatMap(extensionRelief =>
      extensionRelief.giftAid.map(_.reliefAmount)
    ).getOrElse(BigDecimal(0))

    (giftAidSource, giftAidRelief)
  }

  def sortedTaxableIncomes(incomes: TaxCodeIncomes) = {

    val taxableIncomes = incomes.employments.map(_.taxCodeIncomes).getOrElse(Nil) :::
      incomes.occupationalPensions.map(_.taxCodeIncomes).getOrElse(Nil) :::
      incomes.taxableStateBenefitIncomes.map(_.taxCodeIncomes).getOrElse(Nil) :::
      incomes.ceasedEmployments.map(_.taxCodeIncomes).getOrElse(Nil)

    taxableIncomes.sortBy(employment => (employment.employmentType.getOrElse(0) * 1000) + employment.employmentId.getOrElse(0))
  }

  def cyPlusOneAvailable(taxSummaryDetals: TaxSummaryDetails): Boolean = {
    taxSummaryDetals.cyPlusOneChange.isDefined
  }
}
