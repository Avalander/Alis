package alis

import token._

object Eval {
  def evalAst (ast: List[Token], env: Env): List[Token] =
    ast map (evalToken(_, env))
  
  private def evalToken(token: Token, env: Env): Token =
    token match {
      case Atom(x)                  => env(x)
      case ListToken(Atom(x) :: xs) => env(x, xs map (evalToken(_, env)))
      case LitListToken(xs)         => LitListToken(xs map (evalToken(_, env)))
      case x                        => x
    }
}
