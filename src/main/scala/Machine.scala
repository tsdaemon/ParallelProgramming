package com.tsdaemon.homework3

/**
  * Created by Anatoliy on 13.05.2017.
  */
final class Machine {
  def run(statement: Statement, env: Map[String, Any]): Map[String, Any] = {
    println(s"Running statement:\n$statement\nEnvironment:\n$env\n\n")

    statement match {
      case DoNothing => env

      case Assign(name, expr) =>
        if(expr.isReduciable) assignRun(name, reduce(expr, env), env)
        else assignRun(name, expr, env)

      case IfElseStatement(cond, ifSt, elseSt) =>
        if(cond.isReduciable) ifElseRun(reduce(cond, env), ifSt, elseSt, env)
        else ifElseRun(cond, ifSt, elseSt, env)

      case WhileLoop(cond, statement) => whileRun(cond, statement, env)

      case Block(ls) => ls.foldRight(env)(run)
    }
  }

  def whileRun(cond: Expr, statement: Statement, env: Map[String, Any]):Map[String, Any] = reduce(cond, env) match {
    case Bool(b) =>
      if(b) run(WhileLoop(cond, statement), run(statement, env))
      else run(DoNothing, env)
    case _ => throw new Exception("While loop condition reduced not to Bool type")
  }

  def ifElseRun(cond: Expr, ifSt: Statement, elseSt: Statement, env: Map[String, Any]): Map[String, Any] = cond match {
    case Bool(b) => if(b) run(ifSt, env) else run(elseSt, env)
    case _ => throw new Exception("Condition expression type is not Bool")
  }

  def getValue(expr: Expr):Any = expr match {
    case Bool(b) => b
    case Integer(i) => i
    case _ => throw new Exception("Can not get value of not reduced expression")
  }

  def assignRun(name: String, expr: Expr, env: Map[String, Any]):Map[String, Any] = {
    if(env contains name) env - name + (name -> getValue(expr))
    else env + (name -> getValue(expr))
  }

  def reduce(expr:Expr, env: Map[String, Any]):Expr = {
    println(s"Reducing: $expr")

    if(expr.isReduciable)
      reduce(reductionStep(expr, env), env)
    else {
      println("\n")
      expr
    }
  }

  def reductionStep(expr: Expr, env: Map[String, Any]): Expr = expr match {
    case Sum(lOp, rOp) => reductionStepSum(lOp, rOp, env)
    case Prod(lOp, rOp) => reductionStepProd(lOp, rOp, env)
    case Var(name) => reductionStepVar(name, env)
    case Less(lOp, rOp) => reductionStepLess(lOp, rOp, env)
    case IfElse(c, lOp, rOp) => reductionStepIfElse(c, lOp, rOp, env)
  }

  def reductionStepIfElse(c: Expr, ifExpr: Expr, elseExpr: Expr, env: Map[String, Any]) = {
    if(c.isReduciable) IfElse(reductionStep(c, env), ifExpr, elseExpr)
    else c match {
      case Bool(b) => if(b) ifExpr else elseExpr
      case _ => throw new Exception("IfElse condition type is not Bool")
    }
  }

  def reductionStepLess(lOp: Expr, rOp: Expr, env: Map[String, Any]) = {
    if (lOp.isReduciable) Less(reductionStep(lOp, env), rOp)
    else if(rOp.isReduciable) Less(lOp, reductionStep(rOp, env))
    else lOp match {
      case Integer(l) => rOp match {
        case Integer(r) => Bool(l<r)
        case _ => throw new Exception("Invalid right operand type for less operation")
      }
      case _ => throw new Exception("Invalid left operand type for less operation")
    }
  }

  def reductionStepSum(lOp:Expr, rOp:Expr, env: Map[String, Any]): Expr = {
    if (lOp.isReduciable) Sum(reductionStep(lOp, env), rOp)
    else if(rOp.isReduciable) Sum(lOp, reductionStep(rOp, env))
    else lOp match {
      case Integer(l) => rOp match {
        case Integer(r) => Integer(l+r)
        case _ => throw new Exception("Invalid right operand type for sum operation")
      }
      case _ => throw new Exception("Invalid left operand type for sum operation")
    }
  }

  def reductionStepProd(lOp: Expr, rOp: Expr, env: Map[String, Any]) = {
    if (lOp.isReduciable) Prod(reductionStep(lOp, env), rOp)
    else if(rOp.isReduciable) Prod(lOp, reductionStep(rOp, env))
    else lOp match {
      case Integer(l) => rOp match {
        case Integer(r) => Integer(l*r)
        case _ => throw new Exception("Invalid right operand type for prod operation")
      }
      case _ => throw new Exception("Invalid left operand type for prod operation")
    }
  }

  def reductionStepVar(name: String, env: Map[String, Any]) = {
    if(env.contains(name)) env(name) match {
      case i:Int => Integer(i)
      case b:Boolean => Bool(b)
      case _ => throw new Exception(s"Variable $name type is unknown")
    }
    else throw new Exception(s"Variable $name is not defined")
  }
}