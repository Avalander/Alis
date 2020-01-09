package alis

import scala.collection.mutable.Map

import token._

class Env {
  type Func = (List[Token]) => Token

  private val env = Map[String, Func](
    "define" -> define,
    "print" -> print_,
    "+" -> add,
    "-" -> sub,
    "*" -> mul,
    "/" -> div,
    "*pi*" -> value(NumberToken(math.Pi))
  )

  def apply(op: String, xs: List[Token]): Token =
    env.get(op) match {
      case None    => throw new Exception(s"Syntax Error: $op is not defined")
      case Some(f) => f(xs)
    }
  
  def apply(key: String): Token =
    env.get(key) match {
      case None    => throw new Exception(s"Syntax Error: $key is not defined")
      case Some(v) => v(Nil)
    }

  private def value = (x: Token) => (xs: List[Token]) => x

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

  private def define: Func = (xs: List[Token]) =>
    xs match {
      case List(Atom(x), t) =>
        if (env.contains(x)) throw new AssignmentError(s"Name $x already bound")
        else {
          env += x -> value(t)
          LitListToken(Nil)
        }
      case List(a, b) => throw new SyntaxError(s"Syntax Error: invalid identifier $a")
      case _          => throw new SyntaxError(s"Syntax Error: define takes two arguments, ${xs.size} provided")
    }
  
  private def print_ : Func = (xs: List[Token]) => {
    println(show(xs.head))
    LitListToken(Nil)
  }

  private def show (t: Token): String =
    t match {
      case BooleanToken(value) => value.toString
      case NumberToken(value)  => value.toString
      case StringToken(value)  => value
      case ListToken(xs)       => "(" + (xs map show) mkString " " + ")"
      case LitListToken(xs)    => "(" + (xs map show) mkString " " + ")"
      case _                   => "'()"
    }

  class SyntaxError(private val message: String) extends Exception(message)

  class AssignmentError(private val message: String) extends Exception(message)
}
