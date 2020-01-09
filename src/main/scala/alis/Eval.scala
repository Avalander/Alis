package alis

import token._

object Eval {
  def evalAst (ast: Seq[Token], env: Env): Seq[Token] =
    ast map (evalToken(_, env))
  
  private def evalToken(token: Token, env: Env): Token =
    token match {
      case Atom(x)                  => env(x)
      case ListToken(Atom("define") :: x :: xs) =>
        env("define", x :: (xs map (evalToken(_, env))))
      case ListToken(Atom(x) :: xs) => env(x, xs map (evalToken(_, env)))
      case LitListToken(xs)         => LitListToken(xs map (evalToken(_, env)))
      case x                        => x
    }
}
