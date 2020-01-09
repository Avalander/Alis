package alis

import scala.annotation.tailrec
import scala.io.StdIn.readLine

import token._
import Reader._
import Printer._
import Eval._

object Alis {
  def read (args: String): Seq[Token] = tokenize(args)

  def eval (args: Seq[Token], env: Env): Seq[Token] = evalAst(args, env)

  def print_ (args: Seq[Token]): String = printStr(args)

  def rep (args: String, env: Env): String =
    print_(eval(read(args), env))

  private val env = new Env

  @tailrec
  def app (): Unit = {
    val input = readLine("λ> ")
    if (input != ":q") {
      val output = rep(input, env)
      println(output)
      app()
    }
  }

  def main (args: Array[String]): Unit = {
    if (args.length == 0) app()
    else {
      println(args mkString " ")
      val env = new Env
      val output = rep(args mkString " ", env)
      println(output)
    }
  }
}
