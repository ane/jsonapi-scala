/*
Copyright (c) 2017, Qvantel
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
 * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
 * Neither the name of the Qvantel nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL Qvantel BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.qvantel.jsonapi.model

import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._
import com.qvantel.jsonapi._
import com.qvantel.jsonapi.spray._

sealed abstract class ResourceLinkage

object ResourceLinkage {
  final case class ToOne(resource: Option[ResourceIdentifierObject]) extends ResourceLinkage
  final case class ToMany(resources: Set[ResourceIdentifierObject])  extends ResourceLinkage

  implicit object ResourceLinkageJsonFormat extends JsonModelFormat[ResourceLinkage] {
    override def write(obj: ResourceLinkage): Json = obj match {
      case ToOne(None)           => JsonNullable
      case ToOne(Some(resource)) => resource.toJsonModel
      case ToMany(resources)     => JsonArray(resources.map(_.toJsonModel).toVector)
    }

    override def read(json: Json): ResourceLinkage = json match {
      case JsonNullable      => ToOne(None)
      case o: JsObject => ToOne(Some(json.as[ResourceIdentifierObject]))
      case a: JsonArray  => ToMany(json.as[Set[ResourceIdentifierObject]])
      case invalid     => deserializationError(s"Invalid resource linkage: ‘$invalid’")
    }
  }
}
