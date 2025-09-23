/*
 * Copyright 2025 Andreas Stenbæk Larsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.Main.CmdOpts
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{ChangeSet, SyntaxTree, Token}
import ca.uwaterloo.flix.language.ast.shared.{AvailableClasses, SecurityContext}
import ca.uwaterloo.flix.language.phase.{Lexer, Parser2, Reader}
import ca.uwaterloo.flix.util.Options

import java.nio.file.Path
import java.util.concurrent.ForkJoinPool

/**
  *
  */
object EffectCount {
  def run(cwd: Path, cmdOpts: CmdOpts): Unit = {
    // configure Flix and add the paths.
    implicit val flix: Flix = new Flix()
    flix.options = Options.TestWithLibNix
    implicit val defaultSctx: SecurityContext = SecurityContext.AllPermissions
    for (file <- cmdOpts.files) {
      val ext = file.getName.split('.').last
      ext match {
        case "flix" => flix.addFlix(file.toPath)
        case _ =>
          Console.println(s"File extension '$ext' is not supported for effect counting.")
          System.exit(1)
      }
    }
    flix.threadPool = new ForkJoinPool(Options.Default.threads)
    val (afterReader, _readerErrors) = Reader.run(flix.getInputs, AvailableClasses.empty)
    val (afterLexer, _lexerErrors) = Lexer.run(afterReader, Map.empty, ChangeSet.Everything)
    val (afterParser, _parserErrors) = Parser2.run(afterLexer, SyntaxTree.empty, ChangeSet.Everything)
    val progTree = afterParser.units.head._2
    Console.println(visitTree(progTree))
    flix.threadPool.shutdown()
  }

  private def visitTree(t: SyntaxTree.Child): Int = {
    var count = 0
    def inner(t: SyntaxTree.Child): Unit = {
      t match {
        case SyntaxTree.Tree(SyntaxTree.TreeKind.Type.Effect, children, _) =>
          count += 1
          children.foreach(inner)
        case SyntaxTree.Tree(_, children, _) => children.foreach(inner)
        case Token(_, _, _, _, _, _) => ()
      }
    }
    inner(t)
    count
  }
}
