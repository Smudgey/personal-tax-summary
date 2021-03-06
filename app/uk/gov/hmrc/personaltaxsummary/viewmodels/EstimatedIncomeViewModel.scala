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

package uk.gov.hmrc.personaltaxsummary.viewmodels

import play.api.libs.json._
import uk.gov.hmrc.model.{nps2, TaxBand, TaxComponent}


case class EstimatedIncomeViewModel(
                                     increasesTax: Boolean = false,
                                     incomeTaxEstimate: BigDecimal = 0,
                                     incomeEstimate: BigDecimal = 0,
                                     taxFreeEstimate: BigDecimal = 0,
                                     taxRelief: Boolean = false,
                                     taxCodes: List[String] = List(),
                                     potentialUnderpayment:Boolean = false,
                                     additionalTaxTable: List[(String,String)] = List(),
                                     additionalTaxTableV2: List[AdditionalTaxRow] = List(),
                                     additionalTaxTableTotal: String = "",
                                     reductionsTable: List[(String,String,String)] = List(),
                                     reductionsTableTotal: String = "",
                                     graph: BandedGraph,
                                     hasChanges: Boolean = false,
                                     ukDividends: Option[TaxComponent],
                                     taxBands: Option[List[nps2.TaxBand]],
                                     incomeTaxReducedToZeroMessage: Option[String],
                                     nextYearTaxTotal: BigDecimal =0,
                                     hasPSA: Boolean = false,
                                     hasSSR: Boolean = false,
                                     newGraph:BandedGraph,
                                     taxRegion: String
                                   )

case class BandedGraph(
                        id:String,
                        bands:List[Band] = List(),
                        minBand :BigDecimal =0,
                        nextBand :BigDecimal = 0,
                        incomeTotal:BigDecimal = 0,
                        zeroIncomeAsPercentage: BigDecimal =0,
                        zeroIncomeTotal: BigDecimal =0,
                        incomeAsPercentage: BigDecimal =0,
                        taxTotal:BigDecimal =0,
                        nextBandMessage: Option[String] = None
                      )

case class Band(
                 colour:String,
                 barPercentage: BigDecimal = 0,
                 tablePercentage: String = "0",
                 income: BigDecimal = 0,
                 tax: BigDecimal = 0,
                 bandType: String
               )

case class AdditionalTaxRow(
                             description:String,
                             amount:String,
                             url:Option[String] = None
                           )

object AdditionalTaxRow {
  implicit val format = Json.format[AdditionalTaxRow]
}

object Band {
  implicit val format = Json.format[Band]
}

object BandedGraph {
  implicit val format = Json.format[BandedGraph]
}

object EstimatedIncomeViewModel {
import TupleFormats._

  implicit val format = Json.format[EstimatedIncomeViewModel]
}

