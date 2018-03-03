package com.qvantel.jsonapi.spray

import com.qvantel.jsonapi.model._
import spray.json.{JsNumber, JsObject, JsValue, JsonFormat, JsonReader, JsonWriter}


trait SprayJsonModel {
  implicit object sprayJsonPrinter extends JsonPrinter {
    override def pretty(json: Json): String =
      converter.fromJson(json).prettyPrint
    override def compact(json: Json): String =
      converter.fromJson(json).compactPrint
  }

  implicit object sprayJsObjectJsonObject extends JsonConvertible[JsObject] {
    override def fromJson(json: Json): JsObject =
      JsObject(json.asJsonObject.fields.toMap.mapValues(converter.fromJson(_)))

    override def toJsonRepr(source: JsObject): JsonObject =
      JsonObject(source.fields.mapValues(converter.toJsonRepr(_)))
  }

  implicit object sprayNumberToJsonNumber extends JsonConvertible[JsNumber] {
    override def toJsonRepr(source: JsNumber): JsonNumber = JsonNumber(source.value)

    override def fromJson(json: Json): JsNumber = JsNumber(json.asJsonNumber.value)
  }



  implicit object sprayJsValueToJson extends JsonConvertible[JsValue] {
    val jsObjectConverter: JsonConvertible[JsObject] = implicitly[JsonConvertible[JsObject]]
    val jsNumberConverter: JsonConvertible[JsNumber] = implicitly[JsonConvertible[JsNumber]]

    override def toJsonRepr(source: JsValue): Json =
      source match {
        case jsObject: JsObject => jsObjectConverter.toJsonRepr(jsObject)
        case jsNumber: JsNumber => jsNumberConverter.toJsonRepr(jsNumber)
        case _                  => throw new RuntimeException("hellooo!")
      }

    override def fromJson(json: Json): JsValue =
      json match {
        case jsonObject: JsonObject => jsObjectConverter.fromJson(jsonObject)
        case jsonNumber: JsonNumber => jsNumberConverter.fromJson(jsonNumber)
        case _                      => throw new RuntimeException("BOOYAAA!")
      }
  }

  def converter: JsonConvertible[JsValue] = implicitly[JsonConvertible[JsValue]]

  implicit def sprayJsonModelFormat[T](implicit F: JsonFormat[T]): JsonModelFormat[T] = new JsonModelFormat[T] {
    override def write(obj: T): Json = Json.from(F.write(obj))

    override def read(json: Json): T = F.read(json.to[JsValue])
  }
}

object SprayJsonModel extends SprayJsonModel