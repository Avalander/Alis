package alis

import org.scalatest._

import Reader._

class ReaderTest extends FunSuite {
  trait Program {
    val function = "(defn add (a b) (+ a b))"
    val program = """(defn loop ((n 1))
    |  (if (> n 10)
    |    '()
    |    (cons n
    |    (loop (+ n 1)))))
    """.stripMargin
    val concat = """(defn greet (name) (+ "hello, \"" name "\""))"""
  }

  // test("readStr parses a simple function") {
  //   new Program {
  //     assert(readStr(function) == List("(", "defn", "add", "(", "a", "b", ")", "(", "+", "a", "b", ")", ")"))
  //   }
  // }

  // test("readStr parses a multiline function") {
  //   new Program {
  //     assert(readStr(program) == List(
  //       "(", "defn", "loop", "(", "(", "n", "1", ")", ")",
  //       "(", "if", "(", ">", "n", "10", ")",
  //       "'(", ")",
  //       "(", "cons", "n",
  //       "(", "loop", "(", "+", "n", "1", ")", ")", ")", ")", ")"
  //     ))
  //   }
  // }

  test("tokenize parses a simple function") {
    new Program {
      assert(tokenize(function) == List(
        OpenParens, Atom("defn"), Atom("add"), OpenParens, Atom("a"), Atom("b"), CloseParens, OpenParens, Atom("+"), Atom("a"), Atom("b"), CloseParens, CloseParens
      ))
    }
  }

  test("tokenize parses strings correctly") {
    new Program {
      assert(tokenize(concat) == List(
        OpenParens, Atom("defn"), Atom("greet"), OpenParens, Atom("name"), CloseParens, OpenParens, Atom("+"), StringToken("""hello, \""""), Atom("name"), StringToken("""\""""), CloseParens, CloseParens
      ))
    }
  }

  test("tokenize parses a multiline function") {
    new Program {
      assert(tokenize(program) == List (
        OpenParens, Atom("defn"), Atom("loop"), OpenParens, OpenParens, Atom("n"), NumberToken(1.0), CloseParens, CloseParens,
        OpenParens, Atom("if"), OpenParens, Atom(">"), Atom("n"), NumberToken(10.0), CloseParens,
        OpenParensLit, CloseParens,
        OpenParens, Atom("cons"), Atom("n"),
        OpenParens, Atom("loop"), OpenParens, Atom("+"), Atom("n"), NumberToken(1.0), CloseParens, CloseParens, CloseParens, CloseParens, CloseParens
      ))
    }
  }
}
