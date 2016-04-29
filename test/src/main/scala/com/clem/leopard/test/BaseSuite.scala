package com.clem.leopard.test

import org.junit.runner.RunWith
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, FunSuite}


@RunWith(classOf[JUnitRunner])
abstract class BaseSuite
  extends FunSuite
  with BeforeAndAfter
  with BeforeAndAfterAll
  with Matchers
