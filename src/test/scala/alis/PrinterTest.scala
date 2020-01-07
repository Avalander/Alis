package alis

import org.scalatest._

import token._
import Printer._
import Reader._

import fixtures._

class PrinterTest extends FunSuite {
  test("parse simple function") {
    new Program {
      val result = printStr(tokenize(function))
      assert(result == function)
    }
  }

  test("parse string literals") {
    new Program {
      val result = printStr(tokenize(concat))
      assert(result == concat)
    }
  }

  test("parse multiline function") {
    new Program {
      val result = printStr(tokenize(program))
      assert(result == "(defn loop ((n 1.0)) (if (> n 10.0) '() (cons n (loop (+ n 1.0)))))")
    }
  }
}
