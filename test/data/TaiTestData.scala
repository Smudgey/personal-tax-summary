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

package data

import java.io.File

import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.model.TaxSummaryDetails

import scala.io.BufferedSource

trait TaiTestData {
  private val basePath = "test/data/"

  private lazy val currentYearTaxSummaryFilename = "SessionDetails/CurrentYearTaxSummaryDetails.json"
  private lazy val everythingTaxSummaryFilename = "Everything/TaxSummary.json"
  private lazy val nonCodedTaxSummaryFilename = "NonCodedIncome/TaxSummary.json"
  private lazy val outstandingDebtFilename = "OutstandingDebt/TaxSummary.json"
  private lazy val potentialUnderpaymentFilename = "PotentialUnderpayment/TaxSummary.json"
  private lazy val reductionsEqualToIncomeTaxLiabilityFilename = "EstimatedIncome/ReductionsEqualToIncomeTaxLiabilityTaxSummary.json"
  private lazy val reductionsGreaterThanIncomeTaxLiabilityFilename = "EstimatedIncome/ReductionsGreaterThanIncomeTaxLiabilityTaxSummary.json"
  private lazy val reductionsLessThanIncomeTaxLiabilityFilename = "EstimatedIncome/ReductionsLessThanIncomeTaxLiabilityTaxSummary.json"

  private def getTaxSummary(fileName: String): TaxSummaryDetails = {
    val jsonFilePath = basePath + fileName
    val file: File = new File(jsonFilePath)

    val source: BufferedSource = scala.io.Source.fromFile(file)
    val jsVal = Json.parse(source.mkString(""))
    val result = Json.fromJson[TaxSummaryDetails](jsVal)
    result.get
  }

  def currentYearTaxSummary: TaxSummaryDetails = getTaxSummary(currentYearTaxSummaryFilename)
  def everythingTaxSummary: TaxSummaryDetails = getTaxSummary(everythingTaxSummaryFilename)
  def nonCodedTaxSummary: TaxSummaryDetails = getTaxSummary(nonCodedTaxSummaryFilename)
  def outstandingDebtTaxSummary: TaxSummaryDetails = getTaxSummary(outstandingDebtFilename)
  def potentialUnderpaymentTaxSummary: TaxSummaryDetails = getTaxSummary(potentialUnderpaymentFilename)
  def reductionsEqualToIncomeTaxLiabilityTaxSummary: TaxSummaryDetails = getTaxSummary(reductionsEqualToIncomeTaxLiabilityFilename)
  def reductionsGreaterThanIncomeTaxLiabilityTaxSummary: TaxSummaryDetails = getTaxSummary(reductionsGreaterThanIncomeTaxLiabilityFilename)
  def reductionsLessThanIncomeTaxLiabilityTaxSummary: TaxSummaryDetails = getTaxSummary(reductionsLessThanIncomeTaxLiabilityFilename)

}
