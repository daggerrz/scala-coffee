package org.scoffeescript

import org.mozilla.javascript._
import java.io.InputStreamReader

/**
 * A Scala / Rhino Coffeescript compiler.
 */
object Compiler {

  /**
   * Compiles a string of Coffeescript code to Javascript.
   *
   * @param code the Coffeescript code
   * @param sourceName a descriptive name for the code unit under compilation (e.g a filename)
   * @param bare if true, no function wrapper will be generated
   * @return the compiled Javascript code
   */
  def compile(code: String, sourceName: Option[String] = None, bare : Boolean = false)
    : Either[CompilationError, String] = withContext { ctx =>
    val scope = ctx.initStandardObjects()
    ctx.evaluateReader(scope,
      new InputStreamReader(
        new java.net.URL("http://jashkenas.github.com/coffee-script/extras/coffee-script.js").openStream(), "UTF-8"
      ), "coffee-script.js", 1, null
    )

    val coffee = scope.get("CoffeeScript", scope).asInstanceOf[NativeObject]
    val compileFunc = coffee.get("compile", scope).asInstanceOf[Function]
    val opts = ctx.evaluateString(scope, "({bare: %b});".format(bare), null, 1, null)

    try {
      Right(compileFunc.call(ctx, scope, coffee, Array(code, opts)).asInstanceOf[String])
    } catch {
      case e : JavaScriptException =>
        Left(CompilationError(sourceName, e.getValue.toString))
    }
  }

  def withContext[T](f: Context => T): T = {
    val ctx = Context.enter()
    try {
      ctx.setOptimizationLevel(-1) // Do not compile to byte code (max 64kb methods)
      f(ctx)
    } finally {
      Context.exit()
    }
  }
}

case class CompilationError(sourceName: Option[String], message: String)
