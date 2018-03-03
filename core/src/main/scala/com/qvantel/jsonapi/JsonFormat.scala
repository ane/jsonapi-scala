package com.qvantel.jsonapi

import scala.annotation.implicitNotFound

trait JsonModelReader[T] {
  def read(json: Json): T
}

trait JsonModelWriter[T] {
  def write(obj: T): Json
}

/**
  * A trait that enables reading and writing objects
  * from Json to strings and vice versa.
  * @since 9.0.0
  */
@implicitNotFound("You are missing a spray-json format instance for this type!")
trait JsonModelFormat[T] extends JsonModelReader[T] with JsonModelWriter[T]
