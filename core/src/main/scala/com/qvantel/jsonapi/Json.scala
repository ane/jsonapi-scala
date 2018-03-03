package com.qvantel.jsonapi

import scala.collection.immutable

/**
  * An abstract representation of a JSON value that decouples
  * the model from any JSON backend, like Spray or Circe.
  *
  * @since 9.0.0
  */
sealed trait Json {
  def toString: String
  def pretty(implicit P: JsonPrinter): String  = P.pretty(this)
  def compact(implicit P: JsonPrinter): String = P.compact(this)

  /** Try to convert this Json into T.
    *
    * @param R an implicit JsonReader for T
    * @tparam T the object to parse
    * @return
    */
  def as[T](implicit R: JsonModelReader[T]) = R.read(this)

  private[this] def expectedError[T](`type`: String): T =
    deserializationException(s"${this.getClass.getName} is not a JsonObject")

  def asJsonObject: JsonObject   = expectedError("JsonObject")
  def asJsonArray: JsonArray     = expectedError("JsonArray")
  def asJsonNumber: JsonNumber   = expectedError("JsonNumber")
  def asJsonString: JsonString   = expectedError("JsonString")
  def asJsonBoolean: JsonBoolean = expectedError("JsonBoolean")

  /**
    * Coerce this value back into a more specific backend implementation.
    * @param C the converter implicit
    * @tparam S the destination type
    * @return conversion of this value to S
    */
  def to[S](implicit C: JsonFormatSource[S]): S = C.fromJson(this)
}

object Json {
  def from[S](s: S)(implicit C: JsonFormatSource[S]): Json = C.toJsonRepr(s)
}

trait JsonFormatSource[S] {
  def toJsonRepr(source: S): Json
  def fromJson(json: Json): S
}

final case class JsonObject(fields: Map[String, Json]) extends Json {
  def fields(collect: String*): immutable.Seq[Json] = collect.flatMap(fields.get)(collection.breakOut)

  override def asJsonObject: JsonObject = this
}

object JsonObject {
  def empty: JsonObject                          = JsonObject(Map.empty[String, Json])
  def apply(fields: (String, Json)*): JsonObject = JsonObject(Map(fields: _*))
}

final case class JsonArray(elements: Vector[Json]) extends Json {
  override def asJsonArray: JsonArray = this
}

final case class JsonNumber(value: BigDecimal) extends Json {
  override def asJsonNumber: JsonNumber = this
}

final case class JsonString(value: String) extends Json {
  override def asJsonString: JsonString = this
}

final case class JsonBoolean(value: Boolean) extends Json {
  override def asJsonBoolean: JsonBoolean = this
}

final case object JsonNullable extends Json
