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

import com.qvantel.jsonapi.JsonApiFormat
import com.qvantel.jsonapi.model.TopLevel.IdType

sealed abstract class TopLevel {
  def meta: MetaObject
  def jsonapi: Option[JsonApiInfo]
  def links: Links
}

sealed trait Compound {
  def included: Map[IdType, ResourceObject]
  final def isCompound: Boolean = included.nonEmpty
}

object TopLevel {
  type IdType = (String, String)

  @inline def mkResourceObjectTuple(obj: ResourceObject): (IdType, ResourceObject) =
    ((obj.id.getOrElse(obj.hashCode().toString), obj.`type`), obj)

  @inline def mkResourceObjectMap(jsValue: Json): Map[IdType, ResourceObject] =
    jsValue match {
      case arr: JsonArray => arr.elements.map(json => mkResourceObjectTuple(json.as[ResourceObject])).toMap
      case JsonNull       => Map.empty
      case _            => deserializationError("included/data must be array or null")
    }

  @inline def mkResourceObjectMap(objs: Iterable[ResourceObject]): Map[IdType, ResourceObject] =
    objs.map(mkResourceObjectTuple).toMap

  final case class Single(data: Option[(IdType, ResourceObject)],
                          meta: MetaObject,
                          jsonapi: Option[JsonApiInfo],
                          links: Links,
                          included: Map[IdType, ResourceObject])
      extends TopLevel
      with Compound

  final case class Collection(data: Map[IdType, ResourceObject],
                              meta: MetaObject,
                              jsonapi: Option[JsonApiInfo],
                              links: Links,
                              included: Map[IdType, ResourceObject])
      extends TopLevel
      with Compound

  final case class Errors(meta: MetaObject, jsonapi: Option[JsonApiInfo], links: Links, errors: Set[ErrorObject])
      extends TopLevel

  implicit object SingleJsonFormat extends JsonModelFormat[Single] {
    @inline def mkResourceObjectMap(jsValue: Json): Map[IdType, ResourceObject] =
      jsValue match {
        case arr: JsonArray => arr.elements.map(json => mkResourceObjectTuple(json.as[ResourceObject])).toMap
        case JsonNull       => Map.empty
        case _            => deserializationError("included must be array or null")
      }

    override def write(obj: Single): Json = {
      val builder = Map.newBuilder[String, Json]
      builder += "data" -> obj.data.map(_._2).toJsonModel
      if (obj.meta.nonEmpty) builder += "meta" -> obj.meta.toJsonModel
      obj.jsonapi.foreach(x => builder += "jsonapi" -> x.toJsonModel)
      if (obj.links.nonEmpty) builder += "links"       -> obj.links.toJsonModel
      if (obj.included.nonEmpty) builder += "included" -> obj.included.values.toJsonModel
      JsonObject(builder.result())
    }

    override def read(json: Json): Single = {
      val fields = json.asJsonObject.fields
      Single(
        data = fields
          .get("data")
          .flatMap(_ match {
            case d: JsonObject => Some(mkResourceObjectTuple(d.as[ResourceObject]))
            case JsonNull      => None
            case _           => deserializationError("data must be null or object")
          }),
        meta = fields.get("meta").map(_.as[MetaObject]).getOrElse(Map.empty),
        jsonapi = fields.get("jsonapi").map(_.as[JsonApiInfo]),
        links = fields.get("links").map(Link.convertToLinks).getOrElse(Map.empty),
        included = fields.get("included").map(mkResourceObjectMap).getOrElse(Map.empty)
      )
    }
  }

  implicit object CollectionJsonFormat extends JsonModelFormat[Collection] {
    @inline private[this] def mkResourceObjectMap(jsValue: Json): Map[IdType, ResourceObject] =
      jsValue match {
        case arr: JsonArray => arr.elements.map(json => mkResourceObjectTuple(json.as[ResourceObject])).toMap
        case JsonNull       => Map.empty
        case _            => deserializationError("included/data must be array or null")
      }

    override def write(obj: Collection): Json = {
      val builder = Map.newBuilder[String, Json]
      builder += "data" -> obj.data.values.toJsonModel
      if (obj.meta.nonEmpty) builder += "meta" -> obj.meta.toJsonModel
      obj.jsonapi.foreach(x => builder += "jsonapi" -> x.toJsonModel)
      if (obj.links.nonEmpty) builder += "links"       -> obj.links.toJsonModel
      if (obj.included.nonEmpty) builder += "included" -> obj.included.values.toJsonModel
      JsonObject(builder.result())
    }

    override def read(json: Json): Collection = {
      val fields = json.asJsonObject.fields
      Collection(
        data = fields
          .get("data")
          .map(mkResourceObjectMap)
          .getOrElse(deserializationError(s"Expected ‘data’ in resource object")),
        meta = fields.get("meta").map(_.as[MetaObject]).getOrElse(Map.empty),
        jsonapi = fields.get("jsonapi").map(_.as[JsonApiInfo]),
        links = fields.get("links").map(Link.convertToLinks).getOrElse(Map.empty),
        included = fields.get("included").map(mkResourceObjectMap).getOrElse(Map.empty)
      )
    }
  }

  implicit object ErrorsJsonFormat extends JsonModelFormat[Errors] {
    override def write(obj: Errors): Json = {
      val builder = Map.newBuilder[String, Json]
      builder += "errors" -> obj.errors.toJsonModel
      if (obj.meta.nonEmpty) builder += "meta" -> obj.meta.toJsonModel
      obj.jsonapi.foreach(x => builder += "jsonapi" -> x.toJsonModel)
      if (obj.links.nonEmpty) builder += "links" -> obj.links.toJsonModel
      JsonObject(builder.result())
    }

    override def read(json: Json): Errors = {
      val fields = json.asJsonObject.fields
      Errors(
        errors = fields.get("errors").map(_.as[Set[ErrorObject]]).getOrElse(Set.empty),
        meta = fields.get("meta").map(_.as[MetaObject]).getOrElse(Map.empty),
        jsonapi = fields.get("jsonapi").map(_.as[JsonApiInfo]),
        links = fields.get("links").map(Link.convertToLinks).getOrElse(Map.empty)
      )
    }
  }

  implicit object TopLevelJsonFormat extends JsonModelFormat[TopLevel] {
    override def write(obj: TopLevel): Json = obj match {
      case s: Single     => SingleJsonFormat.write(s)
      case c: Collection => CollectionJsonFormat.write(c)
      case e: Errors     => ErrorsJsonFormat.write(e)
    }

    override def read(json: Json): TopLevel = {
      val fields = json.asJsonObject.fields
      fields.get("errors") map { _ =>
        ErrorsJsonFormat.read(json)
      } getOrElse (fields.get("data") match {
        case Some(JsonArray(_))  => CollectionJsonFormat.read(json)
        case Some(JsonObject(_)) => SingleJsonFormat.read(json)
        case Some(JsonNull)      => SingleJsonFormat.read(json)
        case None              => deserializationError(s"Missing ‘data’ in resource object")
        case invalid           => deserializationError(s"Invalid ‘data’ in resource object")
      })
    }

    override def included(obj: TopLevel): Set[JsonObject] = obj match {
      case s: Single     => s.included.values.map(_.toJsonModel.asJsonObject).toSet
      case c: Collection => c.included.values.map(_.toJsonModel.asJsonObject).toSet
      case e: Errors     => Set.empty
    }

    override def read(primary: Json,
                      included: Map[(String, String), JsonObject],
                      includePaths: Set[String],
                      includePath: String): TopLevel = ???
  }
}
