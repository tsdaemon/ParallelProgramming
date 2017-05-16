package com.tsdaemon.homework3

/**
  * Created by Anatoliy on 13.05.2017.
  */
sealed trait Statement

case object DoNothing extends Statement {
  override def toString: String = s""
}

case class Assign(name: String, expr: Expr) extends Statement {
  override def toString: String = s"var $name = $expr"
}

case class IfElseStatement(condition: Expr, ifStatement: Statement, elseStatement: Statement) extends Statement {
  override def toString: String = s"if($condition) {\n$ifStatement\n} else {\n$elseStatement\n}"
}

case class WhileLoop(condition: Expr, loop: Statement) extends Statement {
  override def toString: String = s"while($condition) do {\n$loop\n}"
}

case class Block(seq:List[Statement]) extends Statement {
  override def toString: String = seq.map(s => s.toString()).mkString("\n")
}
