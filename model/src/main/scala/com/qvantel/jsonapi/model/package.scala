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
package com.qvantel.jsonapi

package object model {
  type MetaObject    = Map[String, Json]
  type Links         = Map[String, Link]
  type Attributes    = Map[String, Json]
  type Relationships = Map[String, RelationshipObject]

  final implicit class JsonMapOps(val underlying: Map[String, Json]) extends AnyVal {
    @inline def getAs[A](name: String)(implicit r: JsonModelReader[Option[A]]): Option[A] =
      underlying.get(name).flatMap(_.as[Option[A]])

    @inline def getAs[A](name: Symbol)(implicit r: JsonModelReader[Option[A]]): Option[A] = getAs(name.name)
  }

  def deserializationException(reason: String, explanation: String = null, throwable: Throwable = null) =
    throw JsonDeserializationException(reason, explanation, throwable)
  def serializationException(reason: String, explanation: String = null, throwable: Throwable = null) =
    throw JsonSerializationException(reason, explanation, throwable)

  implicit class pimpedJsonWriter[T](obj: T)(implicit F: JsonModelWriter[T]) {
    def toJsonModel: Json = F.write(obj)
  }

  implicit object mapJsonModelInstance extends JsonModelWriter[Map[String, Json]] {
    override def write(obj: Map[String, Json]): Json =
  }
}

package model {
  case class JsonDeserializationException(reason: String, explanation: String, throwable: Throwable) extends RuntimeException(s"JSON deserialization error: $reason, caused by $explanation", throwable)
  case class JsonSerializationException(reason: String, explanation: String, throwable: Throwable) extends RuntimeException(s"JSON deserialization error: $reason, caused by $explanation", throwable)
}
