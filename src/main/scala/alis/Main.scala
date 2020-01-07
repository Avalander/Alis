package alis

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Alis extends App {
  def read (args: String): String = args

  def eval (args: String): String = args

  def print (args: String): String = args

  def rep (args: String): String =
    print(eval(read(args)))

  @tailrec
  def app (): Unit = {
    print("user> ")
    val input = readLine()
    val output = rep(input)
    println(output)
    app()
  }

  app()
}
