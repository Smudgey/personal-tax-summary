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

import org.scalatest._
import matchers._
import play.api.Play.current
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._

trait MessageMatchers {
  class ContainWrappedMessage(a:String, b:String, c:Option[String]=None) extends Matcher[List[MessageWrapper]] {
    def apply(left: List[MessageWrapper]): MatchResult = {
      val m: String = Messages(a)
      val d: String = c.map(a => "\"" + a + "\"").getOrElse("")

      MatchResult(
        left.contains(MessageWrapper(m, b, c)),
        s"""List does not contain "$a", "$b", $d""",
        s"""List contains "$a", "$b", $d"""
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

  def beMessage(a: String) = new BeMessage(a)
}

object MessageMatchers extends MessageMatchers
