package clem

import clem.frontend.TigerParser
import junit.framework.{Assert, Test}
import Assert._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.apache.commons.lang.StringEscapeUtils

import scala.clem.frontend.AST
import scala.clem.frontend.AST.Types._
import scala.clem.frontend.AST.Values._
import scala.util.parsing.input.CharSequenceReader


object ParserSpec extends Properties("Parser") {
  property("parse and show are inverse operations") = forAll { (a: Exp) => TigerParser.parseString(a.show) == Some(a)}
}
