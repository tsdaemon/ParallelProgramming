package com.tsdaemon.homework3

import org.scalatest._

/**
  * Created by Anatoliy on 13.05.2017.
  */
class StatementMachineSpec extends FlatSpec with Matchers {
  val machine = new StatementMachine
  val env = Map[String, Any]()

  "statement DoNothing" should "do nothing" in {
    machine.run(DoNothing, env) should be(Map[String, Any]())
  }

  "statement Assign" should "change variable value" in {
    machine.run(Assign("x", Sum(Integer(2), Integer(2))), env) should be(Map[String, Any]("x" -> 4))

    machine.run(Assign("x", Prod(Integer(4), Var("x"))), Map[String, Any]("x" -> 4)) should be(Map[String, Any]("x" -> 16))

    machine.run(Assign("x", Less(Integer(4), Var("x"))), Map[String, Any]("x" -> 16)) should be(Map[String, Any]("x" -> true))

    machine.run(Assign("x", Var("x")), Map[String, Any]("x" -> true)) should be(Map[String, Any]("x" -> true))
  }

  "statement Assign" should "not change variable value if expression contains error" in {
    machine.run(Assign("x", Sum(Bool(true), Integer(2))), env) contains "__error" should be (true)

    machine.run(Assign("x", Sum(Bool(true), Integer(2))), env) contains "x" should be (false)
  }


  "statement IfElse" should "execute first statement if condition is true" in {
    machine.run(IfElseStatement(Bool(true), Assign("x", Integer(1)), Assign("x", Integer(2))), env) should be (Map[String, Any]("x" -> 1))
  }

  "statement IfElse" should "should not execute statement with error" in {
    machine.run(IfElseStatement(Bool(false),
      Assign("x", Integer(1)), Assign("x",
        Sum(Integer(1), Bool(true)))), env) contains "__error" should be (true)

    machine.run(IfElseStatement(Bool(false),
      Assign("x", Integer(1)),
      Block(List(Assign("x", Sum(Integer(1), Bool(true))), Assign("y", Integer(1))))), env) contains "y" should be (false)
  }

  "statement IfElse" should "execute second statement if condition is false" in {
    machine.run(IfElseStatement(Bool(false), Assign("x", Integer(1)), Assign("x", Integer(2))), env) should be (Map[String, Any]("x" -> 2))
  }

  "statement IfElse" should "throw exception if condition type is not Bool" in {
    machine.run(IfElseStatement(Integer(1), Assign("x", Integer(1)), Assign("x", Integer(2))), env)  contains "__error" should be(true)
  }


  "statement WhileLoop" should "execute inner statement until condition is true" in {
    machine.run(WhileLoop(Less(Var("i"), Integer(3)), Block(List(
                                                                    Assign("x", Sum(Var("x"), Integer(-1))),
                                                                    Assign("i", Sum(Var("i"), Integer(1)))
                                                                  ))),
      Map[String, Any]("x" -> 0, "i" -> 1)) should be (Map[String, Any]("x"->(-2), "i"->3))
  }

  "statement WhileLoop" should "do not execute statement if condition is false" in {
    machine.run(WhileLoop(Less(Integer(3), Var("i")), Block(List(
      Assign("x", Sum(Var("x"), Integer(-1))),
      Assign("i", Sum(Var("i"), Integer(1)))
    ))),
      Map[String, Any]("x" -> 0, "i" -> 1)) should be (Map[String, Any]("x" -> 0, "i" -> 1))
  }

  "statement WhileLoop" should "throw exception if statement is erroneous" in {
    machine.run(WhileLoop(Sum(Integer(3), Bool(true)), Block(List(
      Assign("x", Sum(Var("x"), Integer(-1))),
      Assign("i", Sum(Var("i"), Integer(1)))
    ))),Map[String, Any]("x" -> Integer(0), "i" -> 1)) contains "__error" should be(true)
  }

  "statement WhileLoop" should "throw exception if statement type is not bool" in {
    machine.run(WhileLoop(Var("x"), Block(List(
      Assign("x", Sum(Var("x"), Integer(-1))),
      Assign("i", Sum(Var("i"), Integer(1)))
    ))),Map[String, Any]("x" -> Integer(0), "i" -> 1)) contains "__error" should be(true)
  }

  "statement WhileLoop" should "throw exception if loop statement contains error" in {
    machine.run(WhileLoop(Var("x"), Assign("x", Sum(Var("x"), Bool(true)))), Map[String, Any]("x" -> Bool(true))) contains "__error" should be(true)
  }

  "statement Block"  should "execute each statement in list" in {
    machine.run(Block(List(Assign("x", Integer(3)), Assign("y", Bool(true)))), env) should be (Map[String, Any]("x"->3, "y"->true))
  }

  "statement Block"  should "do not executes next statements after error" in {
    machine.run(Block(List(Assign("y", Sum(Bool(true), Integer(3))), Assign("x", Integer(3)))), env) contains "__error" should be(true)
    machine.run(Block(List(Assign("y", Sum(Bool(true), Integer(3))), Assign("x", Integer(3)))), env) contains "x" should be(false)
  }
}
