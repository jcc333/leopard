package clem

import org.specs.matcher.Matchers

class TestSuite {

  import org.junit.runner.RunWith
  import org.scalatest.Matchers
  import org.scalatest.junit.JUnitRunner
  import org.scalatest.mock.MockitoSugar
  import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, FunSuite}
  import org.scalatest.junit.JUnitSuite
  import org.scalatest.prop.Checkers
  import org.scalacheck.Arbitrary._
  import org.scalacheck.Prop._


  @RunWith(classOf[JUnitRunner])
  abstract class BaseSuite
    extends FunSuite
    with BeforeAndAfter
    with BeforeAndAfterAll
    with Matchers
    with Checkers
}
