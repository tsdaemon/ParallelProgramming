package com.tsdaemon.homework3

import org.scalatest._

/**
  * Created by Anatoliy on 13.05.2017.
  */
class MachineSpec extends FlatSpec with Matchers {
  val machine = new Machine
  val env = Map[String,Expr]()

  "expression Integer" should "be reduced to itself" in {
    machine.reduce(Integer(3), env) should be(Integer(3))
  }


  "expression Bool" should "be reduced to itself" in {
    machine.reduce(Bool(true), env) should be(Bool(true))
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


  "expression with Var" should "be reduced to variable value from environment" in {
    machine.reduce(Var("x"), Map[String,Expr]("x" -> Integer(2))) should be(Integer(2))

    machine.reduce(Var("x"), Map[String,Expr]("x" -> Bool(true))) should be(Bool(true))
  }

  "expression with Var" should "throw exception if no value in environment" in {
    intercept[Exception] {
      machine.reduce(Var("x"), env)
    }
  }

//  "expression with Var" should "throw exception if value in environment has unknown type" in {
//    intercept[Exception] {
//      machine.reduce(Var("x"), Map[String,Expr]("x" -> "tralala"))
//    }
//  }


  "expression with Less" should "be reduced to Bool" in {
    val env = Map[String,Expr]("x" -> Integer(3))

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


  "statement DoNothing" should "do nothing" in {
    machine.run(DoNothing, env) should be(Map[String, Expr]())
  }


  "statement Assign" should "change variable value" in {
    machine.run(Assign("x", Sum(Integer(2), Integer(2))), env) should be(Map[String, Expr]("x" -> Integer(4)))

    machine.run(Assign("x", Prod(Integer(4), Var("x"))), Map[String, Expr]("x" -> Integer(4))) should be(Map[String, Expr]("x" -> Integer(16)))

    machine.run(Assign("x", Less(Integer(4), Var("x"))), Map[String, Expr]("x" -> Integer(16))) should be(Map[String, Expr]("x" -> Bool(true)))
  }


  "statement IfElse" should "execute first statement if condition is true" in {
    machine.run(IfElseStatement(Bool(true), Assign("x", Integer(1)), Assign("x", Integer(2))), env) should be (Map[String, Expr]("x" -> Integer(1)))
  }

  "statement IfElse" should "execute second statement if condition is false" in {
    machine.run(IfElseStatement(Bool(false), Assign("x", Integer(1)), Assign("x", Integer(2))), env) should be (Map[String, Expr]("x" -> Integer(2)))
  }

  "statement IfElse" should "throw exception if condition type is not Bool" in {
    intercept[Exception] {
      machine.run(IfElseStatement(Integer(1), Assign("x", Integer(1)), Assign("x", Integer(2))), env)
    }
  }


  "statement WhileLoop" should "execute inner statement until condition is true" in {
    machine.run(WhileLoop(Less(Var("i"), Integer(3)), Block(List(
                                                                    Assign("x", Sum(Var("x"), Integer(-1))),
                                                                    Assign("i", Sum(Var("i"), Integer(1)))
                                                                  ))),
      Map[String, Expr]("x" -> Integer(0), "i" -> Integer(1))) should be (Map[String, Expr]("x"->Integer(-2), "i"->Integer(3)))
  }

  "statement WhileLoop" should "throw exception is condition type is not Bool" in {
    intercept[Exception] {
      machine.run(WhileLoop(Integer(3), DoNothing), env)
    }
  }


  "statement Block"  should "execute each statement in list" in {
    machine.run(Block(List(Assign("x", Integer(3)), Assign("y", Bool(true)))), env) should be (Map[String, Expr]("x"->Integer(3), "y"->Bool(true)))
  }
}
