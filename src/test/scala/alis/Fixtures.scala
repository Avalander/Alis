package alis

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
  }
}
