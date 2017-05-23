package com.tsdaemon.homework3

/**
  * Created by Anatoliy on 13.05.2017.
  */
final class StatementMachine {
  val expressionMachine = new ExpressionMachine()

  def run(statement: Statement, env: Map[String, Any]): Map[String, Any] = {
    println(s"Running statement:\n$statement\nEnvironment:\n$env\n\n")

    try {
      runStatement(statement, env)
    } catch {
      case e:Exception => env + ("__error" -> e.getMessage())
    }
  }

  private def runStatement(statement: Statement, env: Map[String, Any]): Map[String, Any] = {
    statement match {
      case DoNothing => env

      case Assign(name, expr) =>
        if(expr.isReduciable) assignRun(name, expressionMachine reduce(expr, env), env)
        else assignRun(name, expr, env)

      case IfElseStatement(cond, ifSt, elseSt) =>
        if(cond.isReduciable) ifElseRun(expressionMachine reduce(cond, env), ifSt, elseSt, env)
        else ifElseRun(cond, ifSt, elseSt, env)

      case WhileLoop(cond, statement) => whileRun(cond, statement, env)

      case Block(ls) => ls.foldLeft(env)((env, s) => runStatement(s, env))
    }
  }

  private def whileRun(cond: Expr, statement: Statement, env: Map[String, Any]):Map[String, Any] = expressionMachine reduce(cond, env) match {
    case Bool(b) =>
      if(b) runStatement(WhileLoop(cond, statement), runStatement(statement, env))
      else env
    case _ => throw new Exception("While loop condition reduced not to Bool type")
  }

  private def ifElseRun(cond: Expr, ifSt: Statement, elseSt: Statement, env: Map[String, Any]): Map[String, Any] = cond match {
    case Bool(b) => if(b) runStatement(ifSt, env) else runStatement(elseSt, env)
    case _ => throw new Exception("Condition expression type is not Bool")
  }

  private def assignRun(name: String, expr: Expr, env: Map[String, Any]):Map[String, Any] = {
    val value = expr match {
      case Bool(b) => b
      case Integer(i) => i
      case _ => throw new Exception(s"Trying to assing not reduced expression $expr to variable $name")
    }

    if(env contains name) env - name + (name -> value)
    else env + (name -> value)
  }
}