import com.tsdaemon.homework3._


val env = Map[String,Any]("x" -> 1, "y" -> 2)

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