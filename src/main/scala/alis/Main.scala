package alis

import scala.annotation.tailrec
import scala.io.StdIn.readLine

import token._
import Reader._
import Printer._

object Alis extends App {
  def read (args: String): Seq[Token] = tokenize(args)

  def eval (args: Seq[Token], env: Env): Seq[Token] = args

  def print_ (args: Seq[Token]): String = printStr(args)

  def rep (args: String, env: Env): String =
    print_(eval(read(args), env))

  @tailrec
  def app (): Unit = {
    val env = new Env
    print("user> ")
    val input = readLine()
    val output = rep(input, env)
    println(output)
    app()
  }

  app()
}
