package com.tsdaemon.homework3

/**
  * Created by Anatoliy on 19.05.2017.
  */
final class ExpressionMachine {
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

  private def reductionStepIfElse(c: Expr, ifExpr: Expr, elseExpr: Expr, env: Map[String, Any]) = {
    if(c.isReduciable) IfElse(reductionStep(c, env), ifExpr, elseExpr)
    else c match {
      case Bool(b) => if(b) ifExpr else elseExpr
      case _ => throw new Exception("IfElse condition type is not Bool")
    }
  }

  private def reductionStepLess(lOp: Expr, rOp: Expr, env: Map[String, Any]) = {
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

  private def reductionStepSum(lOp:Expr, rOp:Expr, env: Map[String, Any]): Expr = {
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

  private def reductionStepProd(lOp: Expr, rOp: Expr, env: Map[String, Any]) = {
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

  private def reductionStepVar(name: String, env: Map[String, Any]) = {
    if(env.contains(name)) env(name) match {
      case i:Int => Integer(i)
      case b:Boolean => Bool(b)
      case _ => throw new Exception(s"Evironment value type ${env(name).getClass()} is not supported")
    }
    else throw new Exception(s"Variable $name is not defined")
  }
}
