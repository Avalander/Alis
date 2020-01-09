package alis

import token._

class Env {
  type Func = (List[Token]) => Token

  private val env = Map(
    "+" -> add,
    "-" -> sub,
    "*" -> mul,
    "/" -> div
  )

  private val env2 = Map(
    "*pi*" -> NumberToken(math.Pi)
  )

  def apply(op: String, xs: List[Token]): Token =
    env.get(op) match {
      case None    => throw new Exception(s"Syntax Error: $op is not defined")
      case Some(f) => f(xs)
    }
  
  def apply(key: String): Token =
    env2.get(key) match {
      case None    => throw new Exception(s"Syntax Error: $key is not defined")
      case Some(v) => v
    }

  private def add = (xs: List[Token]) => {
    val result = xs.foldLeft(0.0) {
      case (prev, NumberToken(x)) => prev + x
      case _ => throw new SyntaxError(s"Syntax Error: expected Number")
    }
    NumberToken(result)
  }
  private def sub = (xs: List[Token]) => {
    val result = xs match {
      case NumberToken(x) :: rest => rest.foldLeft(x) {
        case (prev, NumberToken(x)) => prev - x
        case _                      => throw new SyntaxError(s"Syntax Error: expected Number")
      }
      case _ => throw new SyntaxError(s"Syntax Error: expected Number")
    }
    NumberToken(result)
  }
  private def mul = (xs: List[Token]) => {
    val result = xs.foldLeft(1.0) {
      case (prev, NumberToken(x)) => prev * x
      case _ => throw new SyntaxError(s"Syntax Error: expected Number")
    }
    NumberToken(result)
  }
  private def div = (xs: List[Token]) => {
    val result = xs match {
      case NumberToken(x) :: rest => rest.foldLeft(x) {
        case (prev, NumberToken(x)) => prev / x
        case _                      => throw new SyntaxError(s"Syntax Error: expected Number")
      }
      case _ => throw new SyntaxError(s"Syntax Error: expected Number")
    }
    NumberToken(result)
  }

  class SyntaxError(private val message: String) extends Exception(message)
}
