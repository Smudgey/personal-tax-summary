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

import uk.gov.hmrc.api.service.Auditor
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personaltaxsummary.config.MicroserviceAuditConnector
import uk.gov.hmrc.personaltaxsummary.connectors.TaiConnector
import uk.gov.hmrc.personaltaxsummary.domain.TaxSummaryContainer
import uk.gov.hmrc.personaltaxsummary.viewmodelfactories.TaxSummaryContainerFactory
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait TaiService extends Auditor {
  val taiConnector: TaiConnector

  def getSummary(nino: Nino, year:Int)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Option[TaxSummaryContainer]] = {
    withAudit("getSummary", Map("nino" -> nino.value, "year" -> year.toString)) {
      taiConnector.taxSummary(nino, year).map {
        case Some(taxSummaryDetails) => Some(TaxSummaryContainerFactory.createObject(nino, taxSummaryDetails))
        case _ => None
      }
    }
  }
}

object LiveTaiService extends TaiService {
  override val taiConnector = TaiConnector
  override val auditConnector: AuditConnector = MicroserviceAuditConnector
}