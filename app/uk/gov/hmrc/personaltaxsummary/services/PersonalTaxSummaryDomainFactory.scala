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
import uk.gov.hmrc.personaltaxsummary.viewmodelfactories.{EstimatedIncomeViewModelFactory, YourTaxableIncomeViewModelFactory}
import uk.gov.hmrc.personaltaxsummary.viewmodels.{EstimatedIncomeViewModel, YourTaxableIncomeViewModel}

trait PersonalTaxSummaryDomainFactory {

  def buildEstimatedIncome(nino: Nino, taxSummaryDetails: TaxSummaryDetails): EstimatedIncomeViewModel = {
    EstimatedIncomeViewModelFactory.createObject(nino, taxSummaryDetails)
  }

  def buildYourTaxableIncome(nino: Nino, taxSummaryDetails: TaxSummaryDetails): YourTaxableIncomeViewModel = {
    YourTaxableIncomeViewModelFactory.createObject(nino, taxSummaryDetails)
  }
}

object PersonalTaxSummaryDomainFactory extends PersonalTaxSummaryDomainFactory