package com.qvantel.jsonapi

trait JsonPrinter {
  /**
    * Writes json in a human-readable and indented format.
    *
    * @since 9.0.0
    * @param json
    * @return
    */
  def pretty(json: Json): String

  /** Writes json in a compact and human-unreadable format.
    *
    * @since 9.0.0
    * @param json
    * @return
    */
  def compact(json: Json): String
}
