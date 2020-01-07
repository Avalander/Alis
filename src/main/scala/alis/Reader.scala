package alis

import scala.util.{Try, Success, Failure}

object Reader {
  sealed trait Token
  case object OpenParens extends Token
  case object CloseParens extends Token
  case object OpenParensLit extends Token
  case class Atom(value: String) extends Token
  case class StringToken(value: String) extends Token
  case class NumberToken(value: Double) extends Token
  case class BooleanToken(value: Boolean) extends Token

  def tokenize (raw: String): Seq[Token] =
    readStr(raw.toList)

  private def readStr (str: List[Char], result: List[Token] = List()): Seq[Token] =
    str match {
      case Nil                       => result.reverse
      case x :: xs if x.isWhitespace => readStr(xs, result)
      case '\'' :: '(' :: xs         => readStr(xs, OpenParensLit :: result)
      case '(' :: xs                 => readStr(xs, OpenParens :: result)
      case ')' :: xs                 => readStr(xs, CloseParens :: result)
      case '"' :: xs                 => {
        val (value, rest) = readString(xs)
        readStr(rest, StringToken(value) :: result)
      }
      case 't' :: 'r' :: 'u' :: 'e' :: xs => readStr(xs, BooleanToken(true) :: result)
      case 'f' :: 'a' :: 'l' :: 's' :: 'e' :: xs => readStr(xs, BooleanToken(false) :: result)
      case xs                        => {
        val (value, rest) = readToken(xs)
        val token = Try(value.toDouble) match {
          case Success(v) => NumberToken(v)
          case Failure(_) => Atom(value)
        }
        readStr(rest, token :: result)
      }
    }

  private def readString (raw: List[Char], result: List[Char] = List()): (String, List[Char]) =
    raw match {
      case Nil               => (result.reverse.mkString, Nil)
      case '\\' :: '"' :: xs => readString(xs, '"' :: '\\' :: result)
      case '"' :: xs         => (result.reverse.mkString, xs)
      case x :: xs           => readString(xs, x :: result)
    }
  
  private def readToken (raw: List[Char], result: List[Char] = List()): (String, List[Char]) =
    raw match {
      case Nil                       => (result.reverse.mkString, Nil)
      case ')' :: xs                 => (result.reverse.mkString, ')' :: xs)
      case x :: xs if x.isWhitespace => (result.reverse.mkString, xs)
      case x :: xs                   => readToken(xs, x :: result)
    }
}
