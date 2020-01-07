package alis

package object token {
  sealed trait Token
  case class Atom(value: String) extends Token
  case class StringToken(value: String) extends Token
  case class NumberToken(value: Double) extends Token
  case class BooleanToken(value: Boolean) extends Token
  case class ListToken(value: List[Token]) extends Token
  case class LitListToken(value: List[Token]) extends Token
}
