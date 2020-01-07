package alis

import scala.util.{Try, Success, Failure}

object Reader {
  sealed trait Token
  case class Atom(value: String) extends Token
  case class StringToken(value: String) extends Token
  case class NumberToken(value: Double) extends Token
  case class BooleanToken(value: Boolean) extends Token
  case class ListToken(value: List[Token]) extends Token
  case class LitListToken(value: List[Token]) extends Token

  def tokenize (raw: String): Seq[Token] =
    readStr(raw.toList)

  private def readStr (str: List[Char], result: List[Token] = List()): Seq[Token] =
    str match {
      case Nil                       => result.reverse
      case x :: xs if x.isWhitespace => readStr(xs, result)
      case '(' :: xs => {
        val (next, rest) = readList(xs)
        readStr(rest, ListToken(next) :: result)
      }
      case '\'' :: '(' :: xs => {
        val (next, rest) = readList(xs)
        readStr(rest, LitListToken(next) :: result)
      }
      case '"' :: xs                 => {
        val (value, rest) = readString(xs)
        readStr(rest, StringToken(value) :: result)
      }
      case xs                        => {
        val (value, rest) = readToken(xs)
        readStr(rest, evalAtom(value) :: result)
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
  
  private def readList (raw: List[Char], result: List[Token] = List()): (List[Token], List[Char]) =
    raw match {
      case Nil                       => throw new Exception("Invalid Syntax")
      case x :: xs if x.isWhitespace => readList(xs, result)
      case '(' :: xs                 => {
        val (next, rest) = readList(xs)
        readList(rest, ListToken(next) :: result)
      }
      case '\'' :: '(' :: xs         => {
        val (next, rest) = readList(xs)
        readList(rest, LitListToken(next) :: result)
      }
      case ')' :: xs                 => (result.reverse, xs)
      case '"' :: xs                 => {
        val (next, rest) = readString(xs)
        readList(rest, StringToken(next) :: result)
      }
      case xs                        => {
        val (next, rest) = readToken(xs)
        readList(rest, evalAtom(next) :: result)
      }
    }
  
  private def evalAtom (value: String): Token =
    value match {
      case "true"  => BooleanToken(true)
      case "false" => BooleanToken(false)
      case xs      => 
        Try(xs.toDouble) match {
          case Success(v) => NumberToken(v)
          case Failure(_) => Atom(value)
        }
    }
}
