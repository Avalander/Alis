package alis

import token._

object Printer {
  def printStr (data: Seq[Token]): String = {
    data map {
      case Atom(x)         => x
      case StringToken(x)  => s""""$x""""
      case NumberToken(x)  => x
      case BooleanToken(x) => x
      case ListToken(x)    => s"(${printStr(x)})"
      case LitListToken(x) => s"'(${printStr(x)})"
    } mkString " "
  }
}
