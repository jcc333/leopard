package clem

import clem.frontend.TigerParser
import scala.util.parsing.input.CharSequenceReader


object App extends Application {
  var ok = true
  val prompt = ">>>"
  def repl(): Unit = {
    val s: String = readLine(prompt)
    val input = new CharSequenceReader(s)
    val result = TigerParser.expr(input) match {
      case TigerParser.Success(t,_) => t.toString
      case TigerParser.NoSuccess(msg,_) => "Could not parse '" + s + "': " + msg
    }
    println(result)
    repl()
  }
}
