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

import uk.gov.hmrc.model.nps2.IabdType._


object TaiConstants {

  val incomeTaxPage = 1
  val incomePage = 2
  val taxFreeAmountPage = 3
  val taxCodePage = 4
  val maxCompareTaxCodes = 4
  val maxTaxCodes = 4
  val maxVisibleTaxCodes = 3
  val nextTaxYearDetails = 1
  val currentTaxYearDetails = 2
  val lastTaxYearDetails = 3
  val claimATaxRefund  = 4

  val notApplicable = "Not applicable"

  val IABD_TYPE_BENEFITS_IN_KIND_TOTAL = Some(BenefitInKind.code)
  val IABD_TYPE_BENEFITS_IN_KIND = List(
    Some(Accommodation.code), Some(Assets.code), Some(AssetTransfer.code), Some(EducationalServices.code), Some(EmployerProvidedProfessionalSubscription.code),
    Some(EmployerProvidedServices.code), Some(Entertaining.code), Some(Expenses.code), Some(IncomeTaxPaidButNotDeductedFromDirectorsRemuneration.code),
    Some(Mileage.code), Some(NonQualifyingRelocationExpenses.code), Some(NurseryPlaces.code), Some(OtherItems.code), Some(PaymentsOnEmployeesBehalf.code),
    Some(PersonalIncidentalExpenses.code), Some(QualfyingRelocationExpenses.code), Some(TravelAndSubsistence.code), Some(VouchersAndCreditCards))

  val IABD_UPDATE_SOURCES = List(Some(BpaReceivedFromSpouseOrCivilPartner.code), Some(CommunityInvestmentTaxCredit.code), Some(GiftsOfSharesToCharity.code),
    Some(RetirementAnnuityPayments.code), Some(Tips.code), Some(Assets.code), Some(AssetTransfer.code), Some(NurseryPlaces))
  val IABD_TYPE_UKDIVIDENDS = 76
  val TAX_FREE_DIVIDEND: BigDecimal = 5000

  val MCI_GATEKEEPER_TYPE = 6
  val MCI_GATEKEEPER_ID = 6
  val MCI_GATEKEEPER_DESCR = "Manual Correspondence Indicator"

  val defaultPrimaryPay = 15000
  val defaultSecondaryPay = 5000
  val higherRateBandIncome = 150000
}
