import com.tsdaemon.homework3._


val env = Map[String,Expr](
  "x" -> Integer(1),
  "y" -> Integer(2))

val machine = new Machine

machine.reduce(
  IfElse(
    Less(
      Var("x"),
      Integer(2)),
    Prod(
      Var("x"),
      Var("y")),
    Integer(5)),
  env)