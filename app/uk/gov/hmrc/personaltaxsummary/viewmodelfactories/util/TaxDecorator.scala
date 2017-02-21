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

/*
package uk.gov.hmrc.personaltaxsummary.viewmodelfactories.util

import play.api.Play.current
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._
import uk.gov.hmrc.model.{Tax, TaxBand}
import uk.gov.hmrc.play.views.helpers.MoneyPounds

import scala.util.Try

case class TaxDecorator (tax:Tax, employmentCount:Int=1, maxDisplayValueIsIncome:Boolean=false, paAmount: BigDecimal=BigDecimal(0)){

  val taxBandDecorator = new TaxBandDecorator(tax, maxDisplayValueIsIncome, paAmount)

  lazy val nextBandDescription :Option[TaxBandDescription] = taxBandDecorator.nextBandDescription
  lazy val taxBandDescriptions :List[TaxBandDescription] =
    taxBandDecorator.taxBandDescriptions(taxFreeMessage=taxBreakdownTaxFreeMessage, taxBandMessageFunc=taxBandKeyDescription)
  lazy val taxFreeAllocationDescriptions :List[TaxBandDescription] =
    taxBandDecorator.taxBandDescriptions(taxFreeMessage=taxFreeAllocationMessage, taxBandMessageFunc=totalLiabilityTaxBandMessage)

  lazy val incomeAsPercentage :BigDecimal = taxBandDecorator.incomeAsPercentage
  lazy val totalIncome :BigDecimal = taxBandDecorator.totalIncome

  lazy val taxBandLabelFirstAmount = taxBandDecorator.taxBandLabelFirstAmount
  lazy val taxBandLabelMiddle = taxBandDecorator.taxBandLabelMiddle
  lazy val taxBandLabelLastAmount = taxBandDecorator.taxBandLabelLastAmount


  lazy val taxBreakdownTaxFreeMessage : Option[String] = {
    if (!taxBandDecorator.hasIncome) {
      None
    }else if (taxBandDecorator.isAllTaxFree) {
      //Some(Messages("tai.taxCalc.taxFreeAmount.all", MoneyPounds(taxBandDecorator.taxFreeAmount, 0).quantity))
      Some(Messages("tai.taxCalcIncome.taxFreeAmount.available", BigDecimal(0),   MoneyPounds(taxBandDecorator.allowReliefDeducts, 0).quantity))

    }else if (taxBandDecorator.hasTaxFreeAmount) {
      Some(Messages("tai.taxCalc.taxFreeAmount.available", MoneyPounds(BigDecimal(0), 0).quantity,
        MoneyPounds(taxBandDecorator.taxFreeAmount, 0).quantity))
    }else if(taxBandDecorator.hasKCode) {
      Some(Messages("tai.taxCalc.taxFreeAmount.extraBurden", MoneyPounds(taxBandDecorator.taxFreeAmount, 0).quantity))
    }else {
      None
    }
  }

  lazy val taxFreeExtraIncomeMessage : Option[String] = {
    val nonZeroBands = taxBandDecorator.adjustedTaxBands.getOrElse(Nil).filter(_.rate == Some(BigDecimal(0)))
    nonZeroBands match {
      case x if (x.size > 0 && taxBandDecorator.isAllTaxFree) => {
        val rate = nonZeroBands(0).rate.getOrElse(BigDecimal(0))
        val startOfBasicRate = nonZeroBands(0).lowerBand.getOrElse(BigDecimal(0))
        Some(Messages("tai.taxCalc.", rate, startOfBasicRate))
      }
      case _ => None
    }
  }


  def taxBandKeyDescription(band : TaxBandDescription, onlyMessage:Boolean, firstMessage:Boolean, lastMessage:Boolean):String = {
    if (onlyMessage) {
      Messages("tai.taxCalcKey.bands.all", band.taxBand.rate.getOrElse(BigDecimal(0)),
        MoneyPounds(band.taxBand.income.getOrElse(BigDecimal(0)), 0).quantity,
        MoneyPounds(band.taxBand.tax.getOrElse(BigDecimal(0)), 0).quantity)
    }else if (firstMessage) {
      Messages("tai.taxCalcKey.bands.first", band.taxBand.rate.getOrElse(BigDecimal(0)),
        MoneyPounds(band.taxBand.income.getOrElse(BigDecimal(0)), 0).quantity,
        MoneyPounds(band.taxBand.tax.getOrElse(BigDecimal(0)), 0).quantity)
    }else if (lastMessage) {
      Messages("tai.taxCalcKey.bands.lastBand", band.taxBand.rate.getOrElse(BigDecimal(0)),
        MoneyPounds(band.taxBand.income.getOrElse(BigDecimal(0)), 0).quantity,
        MoneyPounds(band.taxBand.tax.getOrElse(BigDecimal(0)), 0).quantity)
    }else {
      Messages("tai.taxCalcKey.bands.range", band.taxBand.rate.getOrElse(BigDecimal(0)),
        MoneyPounds(band.taxBand.income.getOrElse(BigDecimal(0)), 0).quantity,
        MoneyPounds(band.taxBand.tax.getOrElse(BigDecimal(0)), 0).quantity)
    }
  }


  private lazy val taxFreeAllocationMessage : Option[String] = {
    if (!taxBandDecorator.hasIncome) {
      None
    }else if (taxBandDecorator.hasTaxFreeAmount) {
      if (employmentCount > 1) {
        Some(Messages("tai.taxCalc.taxFreeAllocation.multiIncome", MoneyPounds(taxBandDecorator.taxFreeAmount, 0).quantity))
      }else {
        Some(Messages("tai.taxCalc.taxFreeAllocation.singleIncome", MoneyPounds(taxBandDecorator.taxFreeAmount, 0).quantity))
      }
    } else if(taxBandDecorator.hasKCode) {
      Some(Messages("tai.taxCalc.taxFreeAllocation.extraBurden", MoneyPounds(taxBandDecorator.taxFreeAmount, 0).quantity))
    }else {
      None
    }
  }

  def totalLiabilityTaxBandMessage(band : TaxBandDescription, onlyMessage:Boolean, firstMessage:Boolean, lastMessage:Boolean):String = {
    if (onlyMessage) {
      Messages("tai.taxCalc.bands.all", band.taxBand.rate.getOrElse(BigDecimal(0)),
        MoneyPounds(band.taxBand.income.getOrElse(BigDecimal(0)), 0).quantity, MoneyPounds(band.taxBand.tax.getOrElse(BigDecimal(0)), 0).quantity)
    }else if (firstMessage) {
      Messages("tai.taxCalc.bands.first", band.taxBand.rate.getOrElse(BigDecimal(0)),
        MoneyPounds(band.taxBand.income.getOrElse(BigDecimal(0)), 0).quantity, MoneyPounds(band.taxBand.tax.getOrElse(BigDecimal(0)), 0).quantity)
    }else if (lastMessage) {
      Messages("tai.taxCalc.bands.lastBand", band.taxBand.rate.getOrElse(BigDecimal(0)),
        MoneyPounds(band.taxBand.income.getOrElse(BigDecimal(0)), 0).quantity, MoneyPounds(band.taxBand.tax.getOrElse(BigDecimal(0)), 0).quantity)
    }else {
      Messages("tai.taxCalc.bands.range", band.taxBand.rate.getOrElse(BigDecimal(0)),
        MoneyPounds(band.taxBand.income.getOrElse(BigDecimal(0)), 0).quantity, MoneyPounds(band.taxBand.tax.getOrElse(BigDecimal(0)), 0).quantity)
    }
  }
}

case class TaxBandLabel(labelAmount: BigDecimal, locationAsPerc : BigDecimal)
case class TaxBandDescription(widthAsPerc : BigDecimal, className : String, description : String, taxBand : TaxBand)

class TaxBandDecorator(startTax:Tax, maxDisplayValueIsIncome:Boolean=false, paAmount: BigDecimal=BigDecimal(0)) {
  private lazy val zeroBaseline = BigDecimal(0)

  lazy val taxBandsZeroRate = startTax.taxBands.map(_.filter { band =>
    band.rate.contains(zeroBaseline)})
  lazy val zeroRateIncome = taxBandsZeroRate.map(band => band.foldLeft(BigDecimal(0))(_ + _.income.getOrElse(BigDecimal(0)))).getOrElse(BigDecimal(0))
  lazy val taxBandsNoZeroRate = startTax.taxBands.map(_.filter { band =>
    band.rate.exists(rate => rate > zeroBaseline)})
  lazy val tax : Tax = startTax.copy(taxBands = taxBandsNoZeroRate)

  lazy val totalIncome = tax.totalIncome.getOrElse(BigDecimal(0))
  lazy val totalTaxableIncome = tax.totalTaxableIncome.getOrElse(BigDecimal(0))
  lazy val allowReliefDeducts = tax.allowReliefDeducts.getOrElse(BigDecimal(0))
  lazy val totalTax :BigDecimal = tax.totalTax.getOrElse(0)


  lazy val taxFreeAdjustment = allowReliefDeducts// if (totalIncome > totalTaxableIncome) totalIncome - totalTaxableIncome else zeroBaseline
  lazy val taxFreeAmount = totalIncome - totalTaxableIncome + zeroRateIncome
  lazy val incomeToUse = if (totalIncome >= totalTaxableIncome) {totalIncome} else{ totalTaxableIncome }

  lazy val adjustedTaxBands :Option[List[TaxBand]]= {
    val filteredTaxBands = tax.taxBands.map(_.filter { band =>
      band.income.exists(income => income > zeroBaseline)
    })

    val taxBandStartPoint = {
      filteredTaxBands match {
        case Some(filteredTax) if (filteredTax.size > 0) => {
          filteredTax.headOption.flatMap(_.lowerBand).getOrElse(zeroBaseline)
        }
        case _ => zeroBaseline
      }
    }


    val highestBand = tax.taxBands
      .map(bands => bands.filter(
        _.upperBand.getOrElse(BigDecimal(0)) == BigDecimal(0)
      ))
      .map(_.foldLeft(BigDecimal(0))(_ + _.lowerBand.getOrElse(BigDecimal(0)))).getOrElse(BigDecimal(0))

    //Now adjust the upper and lower bands to take into account the different start point and the tax free amount
    tax.taxBands.map(_.map(band => band.copy(lowerBand = band.lowerBand.map(value =>

      if(value > BigDecimal(0) && value == highestBand){
        value + (taxFreeAdjustment - paAmount) - taxBandStartPoint
      }else{
        value + taxFreeAdjustment - taxBandStartPoint
      }
    ),
      upperBand = band.upperBand.map(
        value =>
          if(value > BigDecimal(0)){
            if(value == highestBand){
              value + (taxFreeAdjustment - paAmount) - taxBandStartPoint
            }else {
              value + taxFreeAdjustment - taxBandStartPoint
            }
          }
          else {
            BigDecimal(0)
          }
      ))))
  }

  lazy val adjustedTaxBandsEmptyIncomeRemoved = adjustedTaxBands.map(_.filter{band =>
    band.income.exists(income => income > zeroBaseline )
  })
  private lazy val allTaxBandDetails = basicTaxBandDetails()
  private lazy val filteredTaxBandDetails =
    allTaxBandDetails.filter {bandDesc =>
      bandDesc.taxBand.income.exists(income => income > zeroBaseline)
    }

  lazy val nextBandDescription = {
    //find the band where the lowerAmount matches the max display value
    if (filteredTaxBandDetails.size > 0) {
      val lastBand = filteredTaxBandDetails.lastOption.map(_.taxBand.upperBand).flatten
      allTaxBandDetails.find(_.taxBand.lowerBand == lastBand)
    } else {
      //None
      basicTaxBandDetails().headOption
    }
  }


  def taxBandDescriptions(taxFreeMessage : Option[String]=None, taxBandMessageFunc:(TaxBandDescription,
    Boolean,
    Boolean,
    Boolean)=>String) : List[TaxBandDescription] = {
    val taxFree = taxFreeDescription(taxFreeMessage)
    addTaxBandDescription(taxFree ::: filteredTaxBandDetails, taxBandMessageFunc)
  }

  private def taxFreeDescription(taxFreeMessage : Option[String]=None): List[TaxBandDescription] ={
    taxFreeMessage match {
      case Some(taxFree) => {
        List(new TaxBandDescription(calcPercentage(taxFreeAmount), "TaxFree", taxFree,
          TaxBand(income = Some(taxFreeAmount), tax = Some(zeroBaseline),
            lowerBand = Some(zeroBaseline), upperBand = Some(taxFreeAmount), rate = Some(zeroBaseline))))
      }
      case _ => Nil
    }
  }

  private def basicTaxBandDetails(): List[TaxBandDescription] = {
    //Add the descriptions and width to the tax bands

    //Remove all bands with no income but keep the last band in the list
    //get the first band after the last band with income
    val stripZeroBands = adjustedTaxBands.map{bands => {
      val noZeroBands = bands.filter(_.income.getOrElse(BigDecimal(0)) != BigDecimal(0))
      val upperBandVal = noZeroBands.lastOption.map(_.upperBand).flatten
      val bdsLast = bands.filter(_.lowerBand == upperBandVal && upperBandVal != Some(BigDecimal(0)))
      noZeroBands ::: bdsLast
    }
    }

    //Still need the last banc
    stripZeroBands match {
      case Some(taxBand) => {
        //We need the index so that we know what tax band this is
        val bandsWithIndex = taxBand.zipWithIndex
        bandsWithIndex.map { t =>
          val (band, index) = t

          new TaxBandDescription(calcPercentage(band.income.getOrElse(zeroBaseline)), s"Band${index + 1}", "", band)

        }.toList
      }
      case _ => Nil
    }


  }

  private def addTaxBandDescription (bands :List[TaxBandDescription],
                                     taxBandMessageFunc:(TaxBandDescription, Boolean, Boolean, Boolean)=>String) :  List[TaxBandDescription] = {
    val bandsWithIndex = bands.zipWithIndex
    val bandsWithDescriptions = bandsWithIndex.map{t =>
      val (band, index) = t
      if (band.description != ""){ band }
      else {
        index match {
          case 0 if (bands.size == 1) => {
            //Only Tax Band
            band.copy(description = taxBandMessageFunc(band, true, false, false))
          }
          case 0 if (bands.size > 1) => {
            //First Tax Band in a list
            band.copy(description = taxBandMessageFunc(band, false, true, false))
          }
          case x if (bands.size > (x + 1)) =>
            ///Tax Band in the middle of the list
            band.copy(description = taxBandMessageFunc(band, false, false, false))
          case _ =>
            ///Final item in the list
            band.copy(description = taxBandMessageFunc(band, false, false, true))
        }
      }
    }.toList
    bandsWithDescriptions
  }

  lazy val hasTaxFreeAmount = taxFreeAmount > 0
  lazy val hasKCode = taxFreeAmount < 0
  lazy val isAllTaxFree = taxFreeAmount >= totalIncome
  lazy val hasIncome = totalIncome > 0

  lazy val minDisplayValue = {
    zeroBaseline
  }

  lazy val maxDisplayValue : BigDecimal = {
    if (maxDisplayValueIsIncome) {
      incomeToUse
    } else {

      if(allowReliefDeducts > incomeToUse){
        allowReliefDeducts
      }else{
        //The max value is the either the income or highest upper value in the bands
        adjustedTaxBandsEmptyIncomeRemoved.map(_.foldLeft(incomeToUse)
        ((maxValue, taxBand) => {
          //ignore any bands with zero income
          val upperBand = taxBand.upperBand.getOrElse(BigDecimal(0))
          if (upperBand > maxValue) upperBand else maxValue
        })
        ).getOrElse(incomeToUse)
      }
    }
  }

  private def calcPercentage(valToPerc:BigDecimal ) :BigDecimal = {
    val barRange = maxDisplayValue - minDisplayValue
    val adjustedValue = valToPerc - minDisplayValue
    val adjustedValueAsPerc =  Try((adjustedValue / barRange) * 100).getOrElse(BigDecimal(0))
    adjustedValueAsPerc.setScale(2, BigDecimal.RoundingMode.FLOOR)
  }

  lazy val taxBandLabelFirstAmount : BigDecimal = if (taxBandLabels.size > 0) taxBandLabels(0).labelAmount else BigDecimal(0)
  lazy val taxBandLabelMiddle : List[TaxBandLabel] = if (taxBandLabels.size > 2) taxBandLabels.drop(1).dropRight(1) else Nil
  lazy val taxBandLabelLastAmount : BigDecimal = if (taxBandLabels.size > 1) {
    taxBandLabels.lastOption.map( _.labelAmount).getOrElse(maxDisplayValue)
  } else {
    maxDisplayValue
  }

  lazy val MAX_PERCENTAGE = 100
  lazy val taxBandLabels: List[TaxBandLabel] = {
    //Convert the taxBands into a list of values
    adjustedTaxBandsEmptyIncomeRemoved match {
      case Some(adjusted) => {
        //Add all the adjusted upper bands
        val rateBands = (List(zeroBaseline, maxDisplayValue, taxFreeAdjustment) :::
          adjusted.map(_.upperBand.getOrElse(zeroBaseline)).toList).distinct.sorted
        val taxBandsWithLocations = rateBands.map(x => TaxBandLabel(x, calcPercentage(x)))
        //Remove anything outside the bounds of the graph
        taxBandsWithLocations.filter(rateBand => rateBand.locationAsPerc >= zeroBaseline
          && rateBand.locationAsPerc <= BigDecimal(MAX_PERCENTAGE)).toList
      }
      case _ => Nil
    }
  }

  lazy val incomeAsPercentage :BigDecimal = {
    calcPercentage(totalIncome)
  }
}
*/
