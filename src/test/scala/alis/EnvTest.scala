package alis

import org.scalatest._

import token._

class EnvTest extends FunSuite {
    test("define binds a value to a name") {
        val env = new Env
        env("define", List(Atom("a"), NumberToken(42.0)))
        assert(env("a") == NumberToken(42.0))
    }
}