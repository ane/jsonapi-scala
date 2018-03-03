package com.qvantel.jsonapi.spray

import org.specs2.mutable.Specification
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._
import com.qvantel.jsonapi._

class SprayJsonModelSpec extends Specification {
  "SprayJsonModel" should {
    "print the same thing" in {
      val jsObject = JsObject("foo" -> JsNumber(123123))
      val jsonObject = JsonObject("foo" -> JsonNumber(123123))

      jsonObject.pretty must_== jsObject.prettyPrint
      jsonObject.compact must_== jsObject.compactPrint
    }

    "convert back and forth" in {
      val jsObject = JsObject("foo" -> JsNumber(123123))
      val jsonObject = JsonObject("foo" -> JsonNumber(123123))

      jsonObject.to[JsObject] must_== jsObject
      Json.from[JsObject](jsObject) must_== jsonObject
    }

    "seamlessly use the spray backend" in {
      case class Foo(x: Int, y: Option[Int])
      implicit val fmt = jsonFormat2(Foo)
      val obj = Foo(3, Some(4))
      val asd = obj.toJsonModel.as[Foo]

      asd must_== obj
    }

    "work with maps" in {
      val foo = Map("asd" -> 1)
      val bar = foo.toJsonModel.as[Map[String, Int]]

      foo must_== bar
    }
  }
}
