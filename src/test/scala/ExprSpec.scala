package com.tsdaemon.homework3
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Anatoliy on 22.03.2017.
  */
class ExprSpec extends FlatSpec
  with Matchers {

  "toString result" should "has minimum number of parentheses" in {
    Sum(Prod(Integer(2), Var("x")), Var("y")).toString should be ("2 * x + y")

    Prod(Sum(Integer(2), Var("x")), Var("y")).toString should be ("(2 + x) * y")
  }
}
