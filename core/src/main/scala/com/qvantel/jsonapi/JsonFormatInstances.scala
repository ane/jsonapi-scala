package com.qvantel.jsonapi

trait JsonFormatInstances {
  implicit def mapJsonFormat[K: JsonModelFormat, V: JsonModelFormat]: JsonModelFormat[Map[K, V]] =
    new JsonModelFormat[Map[K, V]] {
      override def write(obj: Map[K, V]): Json =
        JsonObject {
          obj map { f =>
            f._1.toJsonModel match {
              case JsonString(x) => x -> f._2.toJsonModel
              case _             => throw new RuntimeException("Only string keys supported!")
            }
          }
        }

      override def read(json: Json): Map[K, V] =
        json match {
          case jsObj: JsonObject => jsObj.fields.map(f => JsonString(f._1).as[K] -> f._2.as[V])(collection.breakOut)
          case _                 => throw new RuntimeException(s"Reading a Map but ${json} is not a JsonObject")
        }
    }

  implicit def listFormat[T: JsonModelFormat] = new JsonModelFormat[List[T]] {
    override def write(obj: List[T]): Json = JsonArray(obj.map(_.toJsonModel).toVector)

    override def read(json: Json): List[T] =
      json match {
        case JsonArray(xs) => xs.map(_.as[T])(collection.breakOut)
        case x             => deserializationException(s"Can't read a List from other than JsonArray: $x")
      }
  }

  implicit def setFormat[T: JsonModelFormat] = new JsonModelFormat[Set[T]] {
    override def write(obj: Set[T]): Json = JsonArray(obj.map(_.toJsonModel).toVector)
    override def read(json: Json): Set[T] =
      json match {
        case JsonArray(xs) => xs.map(_.as[T]).toSet
      }
  }
}

object JsonFormatInstances extends JsonFormatInstances
