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

  test("tokenize parses a simple function") {
    new Program {
      assert(tokenize(function) == List(
        ListToken(
          List(Atom("defn"), Atom("add"), ListToken(List(Atom("a"), Atom("b"))), ListToken(List(Atom("+"), Atom("a"), Atom("b"))))
        )
      ))
    }
  }

  test("tokenize parses strings correctly") {
    new Program {
      assert(tokenize(concat) == List(
        ListToken(List(
          Atom("defn"),
          Atom("greet"),
          ListToken(List(
            Atom("name")
          )),
          ListToken(List(
            Atom("+"),
            StringToken("""hello, \""""),
            Atom("name"),
            StringToken("""\"""")
          ))
        ))
      ))
    }
  }

  test("tokenize parses a multiline function") {
    new Program {
      assert(tokenize(program) == List(
        ListToken(List(
          Atom("defn"),
          Atom("loop"),
          ListToken(List(
            ListToken(List(
              Atom("n"),
              NumberToken(1.0)
            ))
          )),
          ListToken(List(
            Atom("if"),
            ListToken(List(
              Atom(">"),
              Atom("n"),
              NumberToken(10.0)
            )),
            LitListToken(List()),
            ListToken(List(
              Atom("cons"),
              Atom("n"),
              ListToken(List(
                Atom("loop"),
                ListToken(List(
                  Atom("+"),
                  Atom("n"),
                  NumberToken(1.0)
                ))
              ))
            ))
          ))
        ))
      ))
    }
  }
}
