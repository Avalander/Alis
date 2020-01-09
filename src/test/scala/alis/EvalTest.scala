package alis

import org.scalatest._

import token._
import Eval._

import fixtures._

class EvalTest extends FunSuite {
  test("evalAst simple sum") {
    new Program with WithEnv {
      assert(evalAst(simpleSum, env) == List(NumberToken(3.0)))
    }
  }

  test("evalAst simple subtraction") {
    new Program with WithEnv {
      assert(evalAst(simpleSub, env) == List(NumberToken(-1.0)))
    }
  }

  test("evalAst simple multiplication") {
    new Program with WithEnv {
      assert(evalAst(simpleMul, env) == List(NumberToken(2.0)))
    }
  }

  test("evalAst simple division") {
    new Program with WithEnv {
      assert(evalAst(simpleDiv, env) == List(NumberToken(0.5)))
    }
  }

  test("evalAst arithmetic") {
    new Program with WithEnv {
      assert(evalAst(arithmetic, env) == List(NumberToken(13.0)))
    }
  }

  test("evalAst looks up symbols") {
    new Program with WithEnv {
      assert(evalAst(pie, env) == List(NumberToken(math.Pi + 2.0)))
    }
  }
}