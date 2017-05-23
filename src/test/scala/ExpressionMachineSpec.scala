package com.tsdaemon.homework3

import org.scalatest._

/**
  * Created by Anatoliy on 13.05.2017.
  */
class ExpressionMachineSpec extends FlatSpec with Matchers {
  val machine = new ExpressionMachine
  val env = Map[String, Any]()

  "expression Integer" should "be reduced to itself" in {
    machine.reduce(Integer(3), env) should be(Integer(3))
  }

  "expression Bool" should "be reduced to itself" in {
    machine.reduce(Bool(true), env) should be(Bool(true))
  }

  "expression with Var" should "be reduced to variable value from environment" in {
    machine.reduce(Var("x"), Map[String, Any]("x" -> 2)) should be(Integer(2))

    machine.reduce(Var("x"), Map[String, Any]("x" -> true)) should be(Bool(true))
  }

  "expression with Var" should "throw exception if no value in environment" in {
    intercept[Exception] {
      machine.reduce(Var("x"), env)
    }
  }


  "expression with Sum" should "be reduced to sum of integers" in {
    machine.reduce(Sum(Integer(3), Integer(4)), env) should be(Integer(7))
  }

  "expression with Sum" should "throw an error if left or right operand type is not Integer" in {
    intercept[Exception] {
      machine.reduce(Sum(Bool(true), Integer(4)), env)
    }

    intercept[Exception] {
      machine.reduce(Sum(Integer(3), Bool(true)), env)
    }
  }

  "reductionStep for Sum" should "reduce left operand if it reduciable" in {
    machine.reductionStep(Sum(Var("x"), Integer(4)), Map[String, Any](("x" -> 1))) should be(Sum(Integer(1), Integer(4)))
  }

  "reductionStep for Sum" should "reduce right operand if it reduciable and left is not reduciable" in {
    machine.reductionStep(Sum(Integer(4), Var("x")), Map[String, Any](("x" -> 1))) should be(Sum(Integer(4), Integer(1)))
  }


  "expression with Prod" should "be reduced to product of integers" in {
    machine.reduce(Prod(Integer(3), Integer(4)), env) should be(Integer(12))

    machine.reduce(Prod(Sum(Integer(2), Integer(4)), Integer(4)), env) should be(Integer(24))

    machine.reduce(Sum(Prod(Integer(2), Integer(4)), Integer(4)), env) should be(Integer(12))
  }

  "expression with Prod" should "throw an error if left or right operand type is not Integer" in {
    intercept[Exception] {
      machine.reduce(Prod(Bool(true), Integer(4)), env)
    }

    intercept[Exception] {
      machine.reduce(Prod(Integer(3), Bool(true)), env)
    }
  }

  "reductionStep for Prod" should "reduce left operand if it reduciable" in {
    machine.reductionStep(Prod(Var("x"), Integer(4)), Map[String, Any](("x" -> 1))) should be(Prod(Integer(1), Integer(4)))
  }

  "reductionStep for Prod" should "reduce right operand if it reduciable and left is not reduciable" in {
    machine.reductionStep(Prod(Integer(4), Var("x")), Map[String, Any](("x" -> 1))) should be(Prod(Integer(4), Integer(1)))
  }


  "expression with Less" should "be reduced to Bool" in {
    val env = Map[String, Any]("x" -> 3)

    machine.reduce(Less(Var("x"), Integer(2)), env) should be(Bool(false))

    machine.reduce(Less(Integer(2), Var("x")), env) should be(Bool(true))
  }

  "expression with Less" should "throw an exception if left or right operand type is not Integer" in {
    intercept[Exception] {
      machine.reduce(Less(Integer(3), Bool(true)), env)
    }

    intercept[Exception] {
      machine.reduce(Less(Bool(false), Integer(2)), env)
    }
  }

  "reductionStep for Less" should "reduce left operand if it reduciable" in {
    machine.reductionStep(Less(Var("x"), Integer(4)), Map[String, Any](("x" -> 1))) should be(Less(Integer(1), Integer(4)))
  }

  "reductionStep for Less" should "reduce right operand if it reduciable and left is not reduciable" in {
    machine.reductionStep(Less(Integer(4), Var("x")), Map[String, Any](("x" -> 1))) should be(Less(Integer(4), Integer(1)))
  }


  "expression with IfElse" should "be reduced to first expression if condition is true" in {
    machine.reduce(IfElse(Less(Integer(2),Integer(3)), Bool(true), Integer(5)), env) should be(Bool(true))

    machine.reduce(IfElse(Less(Integer(2),Integer(3)), Sum(Integer(3), Integer(1)), Integer(5)), env) should be(Integer(4))
  }

  "expression with IfElse" should "be reduced to second expression if condition is false" in {
    machine.reduce(IfElse(Less(Integer(3),Integer(2)), Bool(true), Integer(5)), env) should be(Integer(5))

    machine.reduce(IfElse(Less(Integer(3),Integer(2)), Sum(Integer(3), Integer(1)), Integer(5)), env) should be(Integer(5))
  }

  "expression with IfElse" should "throw an error if condition type is not Bool" in {
    intercept[Exception] {
      machine.reduce(IfElse(Integer(4), Bool(true), Integer(5)), env)
    }
  }

  "reductionStep for IfElse" should "reduce condition if it reduciable" in {
    machine.reductionStep(IfElse(Var("x"), Bool(true), Integer(5)), Map[String, Any]("x" -> 2)) should be(IfElse(Integer(2), Bool(true), Integer(5)))
  }
}
