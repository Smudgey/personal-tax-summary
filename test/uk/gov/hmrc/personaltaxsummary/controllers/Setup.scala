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

package uk.gov.hmrc.personaltaxsummary.controllers

import java.util.UUID
import java.util.concurrent.TimeUnit

import data.TaiTestData
import org.scalatest.mock.MockitoSugar
import play.api.libs.json.{JsValue, Json}
import play.api.test.FakeRequest
import play.api.test.Helpers.POST
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personaltaxsummary.domain.{PersonalTaxSummaryContainer, TaxSummaryContainer}
import uk.gov.hmrc.personaltaxsummary.services.PersonalTaxSummaryDomainFactory
import uk.gov.hmrc.personaltaxsummary.viewmodelfactories.{EstimatedIncomeViewModelFactory, TaxSummaryContainerFactory, YourTaxableIncomeViewModelFactory}
import uk.gov.hmrc.play.http.HeaderCarrier

trait Setup extends TaiTestData with MockitoSugar {
  implicit val hc = HeaderCarrier()
  implicit val timeout = akka.util.Timeout(1000, TimeUnit.MILLISECONDS)

  val journeyId: String = UUID.randomUUID().toString
  val nino = Nino("CS700100A")

  val someContainer: TaxSummaryContainer = TaxSummaryContainerFactory.createObject(nino, currentYearTaxSummary)
  val badRequest: FakeRequest[JsValue] = FakeRequest(POST, "/some/url").withBody(Json.toJson(someContainer))
    .withHeaders("Content-Type" -> "application/json")



  val estimatedIncomeViewModel = EstimatedIncomeViewModelFactory.createObject(nino, currentYearTaxSummary)
  val yourTaxableIncomeViewModel = YourTaxableIncomeViewModelFactory.createObject(nino, currentYearTaxSummary)

  val emptyRequest = FakeRequest()
  val taxSummaryDetailsRequest: FakeRequest[JsValue] = FakeRequest(POST, "/some/url").withBody(Json.toJson(currentYearTaxSummary))
    .withHeaders("Content-Type" -> "application/json")


  val domainController = new PersonalTaxSummaryDomainController {
    override val domain: PersonalTaxSummaryDomainFactory = PersonalTaxSummaryDomainFactory
  }
}