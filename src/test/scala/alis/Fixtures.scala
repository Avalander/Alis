package alis

import token._

object fixtures {
  trait Program {
    val function = "(defn add (a b) (+ a b))"
    val program = """(defn loop ((n 1))
    |  (if (> n 10)
    |    '()
    |    (cons n
    |    (loop (+ n 1)))))
    """.stripMargin
    val concat = """(defn greet (name) (+ "hello, \"" name "\""))"""
    val simpleSum = List(ListToken(List(Atom("+"), NumberToken(1.0), NumberToken(2.0))))
    val simpleSub = List(ListToken(List(Atom("-"), NumberToken(1.0), NumberToken(2.0))))
    val simpleMul = List(ListToken(List(Atom("*"), NumberToken(1.0), NumberToken(2.0))))
    val simpleDiv = List(ListToken(List(Atom("/"), NumberToken(1.0), NumberToken(2.0))))
    // (+ 2 (* 3 3) (/ 4 (- 4 2)))
    val arithmetic = List(
      ListToken(List(
        Atom("+"),
        NumberToken(2.0),
        ListToken(List(
          Atom("*"),
          NumberToken(3.0),
          NumberToken(3.0)
        )),
        ListToken(List(
          Atom("/"),
          NumberToken(4.0),
          ListToken(List(
            Atom("-"),
            NumberToken(4.0),
            NumberToken(2.0)
          ))
        ))
      ))
    )
    val pie = List(
      ListToken(List(
        Atom("+"),
        NumberToken(2.0),
        Atom("*pi*")
      ))
    )
  }

  trait WithEnv {
    val env = new Env
  }
}
