package com.qvantel.jsonapi.spray

import com.qvantel.jsonapi.{Json, JsonFormatSource, JsonModelFormat, JsonNumber, JsonObject, JsonPrinter, JsonString}
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._

trait SprayJsonModel {
  implicit object sprayJsonPrinter extends JsonPrinter {
    override def pretty(json: Json): String =
      converter.fromJson(json).prettyPrint
    override def compact(json: Json): String =
      converter.fromJson(json).compactPrint
  }

  implicit object sprayJsObjectJsonObject extends JsonFormatSource[JsObject] {
    override def fromJson(json: Json): JsObject =
      JsObject(json.asJsonObject.fields.toMap.mapValues(converter.fromJson(_)))

    override def toJsonRepr(source: JsObject): JsonObject =
      JsonObject(source.fields.mapValues(converter.toJsonRepr(_)))
  }

  implicit object sprayNumberToJsonNumber extends JsonFormatSource[JsNumber] {
    override def toJsonRepr(source: JsNumber): JsonNumber = JsonNumber(source.value)

    override def fromJson(json: Json): JsNumber = JsNumber(json.asJsonNumber.value)
  }

  implicit object sprayStringToJsonString extends JsonFormatSource[JsString] {
    override def toJsonRepr(source: JsString): JsonString = JsonString(source.value)

    override def fromJson(json: Json): JsString = JsString(json.asJsonString.value)
  }

  implicit object sprayJsValueToJson extends JsonFormatSource[JsValue] {
    val jsObjectConverter: JsonFormatSource[JsObject] = implicitly[JsonFormatSource[JsObject]]
    val jsNumberConverter: JsonFormatSource[JsNumber] = implicitly[JsonFormatSource[JsNumber]]
    val jsStringConverter: JsonFormatSource[JsString] = implicitly[JsonFormatSource[JsString]]

    override def toJsonRepr(source: JsValue): Json =
      source match {
        case jsObject: JsObject   => jsObjectConverter.toJsonRepr(jsObject)
        case jsNumber: JsNumber   => jsNumberConverter.toJsonRepr(jsNumber)
        case jsonString: JsString => jsStringConverter.toJsonRepr(jsonString)
        case _                    => throw new RuntimeException("hellooo!")
      }

    override def fromJson(json: Json): JsValue =
      json match {
        case jsonObject: JsonObject => jsObjectConverter.fromJson(jsonObject)
        case jsonNumber: JsonNumber => jsNumberConverter.fromJson(jsonNumber)
        case jsonString: JsonString => jsStringConverter.fromJson(jsonString)
        case _                      => throw new RuntimeException("BOOYAAA!")
      }
  }

  def converter: JsonFormatSource[JsValue] = implicitly[JsonFormatSource[JsValue]]

  implicit def sprayJsonModelFormat[T](implicit F: JsonFormat[T]): JsonModelFormat[T] = new JsonModelFormat[T] {
    override def write(obj: T): Json = Json.from(F.write(obj))

    override def read(json: Json): T = F.read(json.to[JsValue])
  }
}

object SprayJsonModel extends SprayJsonModel
