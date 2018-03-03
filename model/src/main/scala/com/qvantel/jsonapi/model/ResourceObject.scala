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

final case class ResourceObject(id: Option[String],
                                `type`: String,
                                attributes: Attributes,
                                relationships: Relationships,
                                links: Links,
                                meta: MetaObject)

object ResourceObject {
  implicit object ResourceObjectJsonFormat extends JsonModelFormat[ResourceObject] {
    override def write(obj: ResourceObject): Json = {
      val builder = Map.newBuilder[String, Json]
      obj.id.foreach(x => builder += "id" -> x.toJsonModel)
      builder += "type" -> obj.`type`.toJsonModel
      if (obj.attributes.nonEmpty) builder += "attributes"       -> obj.attributes.toJsonModel
      if (obj.relationships.nonEmpty) builder += "relationships" -> obj.relationships.toJsonModel
      if (obj.links.nonEmpty) builder += "links"                 -> obj.links.toJsonModel
      if (obj.meta.nonEmpty) builder += "meta"                   -> obj.meta.toJsonModel
      JsonObject(builder.result())
    }

    override def read(json: Json): ResourceObject = {
      val fields = json.asJsonObject.fields

      ResourceObject(
        id = fields.get("id").flatMap(_.as[Option[String]]),
        `type` = fields
          .get("type")
          .map(_.as[String])
          .getOrElse(deserializationError(s"No ‘type’ field in resource object")),
        attributes = fields.get("attributes").map(_.as[Map[String, Json]]).getOrElse(Map.empty),
        relationships = fields.get("relationships").map(_.as[Relationships]).getOrElse(Map.empty),
        links = fields.get("links").map(Link.convertToLinks).getOrElse(Map.empty),
        meta = fields.get("meta").map(_.as[MetaObject]).getOrElse(Map.empty)
      )
    }
  }
}
