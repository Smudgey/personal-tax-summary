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

package uk.gov.hmrc.personaltaxsummary.viewmodels

import org.scalatest.matchers._
import play.api.Play.current
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._

trait WrappedDataMatchers {
  class ContainWrappedMessage(a:String, b:String, c:Option[String]=None) extends Matcher[List[MessageWrapper]] {
    def apply(left: List[MessageWrapper]): MatchResult = {
      val m: String = Messages(a)
      val n: Option[String] = c.map(Messages(_))

      MatchResult(
        left.contains(MessageWrapper(m, b, n)),
        s"""List does not contain "$m", "$b", $n""",
        s"""List contains "$m", "$b", $n"""
      )
    }
  }

  class ContainWrappedBenefitsData(a:String, b:String, c:String, d:String, e:Option[Int], f:Option[Int]) extends Matcher[List[BenefitsDataWrapper]] {
    def apply(left: List[BenefitsDataWrapper]): MatchResult = {
      val m: String = if (a.contains(".")) Messages(a) else a
      val n: String = Messages(c)

      MatchResult(
        left.contains(BenefitsDataWrapper(m, b, n, d, e, f)),
        s"""List does not contain "$a", "$b", "$c", "$d", "$e", "$f"""",
        s"""List contains "$a", "$b", "$c", "$d", "$e", "$f""""
      )
    }
  }

  class BeMessage(a: String) extends Matcher[String] {
    def apply(left: String): MatchResult = {
      val m: String = Messages(a)

      MatchResult(
        left.contains(m),
        s"""Message does not contain "$a"""",
        s"""Message contains "$a""""
      )
    }
  }

  def containWrappedMessage(a:String, b:String, c:Option[String]=None) = new ContainWrappedMessage(a, b, c)

  def containWrappedBenefitsData(a:String, b:String, c:String, d:String, e:Option[Int], f:Option[Int]) = new ContainWrappedBenefitsData(a, b, c, d, e, f)

  def beMessage(a: String) = new BeMessage(a)

  def preformat(a: String, b: String) = Messages(a, b)
}

object WrappedDataMatchers extends WrappedDataMatchers
