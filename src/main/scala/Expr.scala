package com.tsdaemon.homework3

/**
  * Created by Anatoliy on 13.05.2017.
  */

sealed trait Expr {
  def isReduciable:Boolean
}

case class IfElse(conditionExpr: Expr, ifExpr: Expr, elseExpr: Expr) extends Expr {
  override def toString: String = s"if($conditionExpr) $ifExpr else $elseExpr"
  override def isReduciable: Boolean = true
}

case class Var(name:String) extends Expr {
  override def toString: String = name
  override def isReduciable: Boolean = true
}

case class Bool(n:Boolean) extends Expr {
  override def toString: String = n.toString()
  override def isReduciable: Boolean = false
}

case class Less(lOp: Expr, rOp:Expr) extends Expr {
  override def toString: String = s"$lOp < $rOp"
  override def isReduciable: Boolean = true
}

case class Integer(n:Int) extends Expr {
  override def toString: String = n.toString()
  override def isReduciable: Boolean = false
}

case class Sum(lOp:Expr, rOp:Expr) extends Expr {
  override def toString: String = s"${lOp.toString()} + ${rOp.toString()}"
  override def isReduciable: Boolean = true
}

case class Prod(lOp:Expr, rOp:Expr) extends Expr {
  override def toString: String = lOp match {
    case Sum(_, _) => rOp match {
      case Sum(_, _) => s"($lOp) * ($rOp)"
      case _ => s"($lOp) * $rOp"
    }
    case _ => rOp match {
      case Sum(_, _) => s"$lOp * ($rOp)"
      case _ => s"$lOp * $rOp"
    }
  }
  override def isReduciable: Boolean = true
}