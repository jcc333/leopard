package com.clem.leopard

import com.clem.leopard.frontend.TigerParser

import scala.io.StdIn
import scala.util.parsing.input.CharSequenceReader

object Repl {
  var ok = true
  val prompt = ">>>"
  def apply(): Unit = {
    val s: String = StdIn.readLine(prompt)
    val input = new CharSequenceReader(s)
    val result = TigerParser.expr(input) match {
      case TigerParser.Success(t,_) => t.toString
      case TigerParser.NoSuccess(msg,_) => "Could not parse '" + s + "': " + msg
    }
    println(result)
    Repl()
  }
}
