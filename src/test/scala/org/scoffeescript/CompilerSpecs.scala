package org.scoffeescript

import org.specs2.mutable._

class CompilerSpecs extends Specification {

  val COFFEE = "alert 'Hello world'"

  /**
   * Strip all whitespace to make none-exact comparisons easier - we don't care about indenting.
   */
  def strip(s: String) = s.replaceAll("\\s", "")

  "compile" should {
    "compile wrapped Coffeescript successfully" in {

      val expected = """
        (function() {
          alert('Hello world');
        }).call(this);
      """
      Compiler.compile(COFFEE) match {
        case Right(js) => strip(js) must_== strip(expected)
      }
    }
    "compile bare Coffeescript successfully" in {
      val expected = """alert('Hello world');"""

      Compiler.compile(COFFEE, bare = true) match {
        case Right(js) => strip(js) must_== strip(expected)
      }
    }
    "return a meaningful error if compilation fails" in {
      Compiler.compile("'", sourceName = Some("test.js")) match {
        case Left(CompilationError(sourceName, message)) =>
          sourceName must_== Some("test.js")
          message must_== "Error: Parse error on line 1: Unexpected '''"
      }
    }
  }
}