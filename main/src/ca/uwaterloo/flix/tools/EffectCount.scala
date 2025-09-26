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
import ca.uwaterloo.flix.language.ast.SyntaxTree.{Child, Tree, TreeKind}
import ca.uwaterloo.flix.language.ast.TokenKind.{NameGreek, NameLowerCase, NameUpperCase, UserDefinedOperator}
import ca.uwaterloo.flix.language.ast.{ChangeSet, SyntaxTree, Token, TokenKind}
import ca.uwaterloo.flix.language.ast.shared.{AvailableClasses, SecurityContext}
import ca.uwaterloo.flix.language.phase.{Lexer, Parser2, Reader}
import ca.uwaterloo.flix.util.Options
import org.json4s.native.JsonMethods.*
import org.json4s.{JField, JInt, JNothing, JObject, JString, JValue, JsonWriter}

import java.nio.file.Path
import java.util.concurrent.ForkJoinPool

/**
  *
  */
object EffectCount {
  var id = 0
  private def nextId(): Int = {
    val x = id
    id = id + 1;
    x
  }

  private def resetId(): Unit = { id = 0 }

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
    val (afterReader, _) = Reader.run(flix.getInputs, AvailableClasses.empty)
    val (afterLexer, _) = Lexer.run(afterReader, Map.empty, ChangeSet.Everything)
    val (afterParser, _) = Parser2.run(afterLexer, SyntaxTree.empty, ChangeSet.Everything)
    val progTree = afterParser.units.head._2
    val toks = ("tokens", JInt(countTokens(afterLexer.head._2)))
    val sigs = ("sigs", JObject(visitTree(progTree)))
    Console.println(pretty(render(JObject(List(toks, sigs)))))
    //printTree(progTree)
    flix.threadPool.shutdown()
  }

  private def countTokens(toks: Array[Token]): Int = {
    toks.foldLeft(0)({case (acc, Token(kind,_,_,_,_,_)) =>
      kind match {
        case TokenKind.CommentDoc
             | TokenKind.CommentBlock
             | TokenKind.CommentLine => acc
        case _ => acc + 1
    }})
  }

  private def visitTree(t: Child): List[JField] = {
    var count = 0

    def inner(t: Child): List[JField] = {
      var obj: List[JField] = List()
      def innerInner(t: Child): Unit = {
        t match {
          case Tree(SyntaxTree.TreeKind.Decl.Module, c, _) => {
            val name = getIdent(c).getOrElse("")
            obj = (name, JObject(c.flatMap(inner).toList)) :: obj
          }
          case Tree(SyntaxTree.TreeKind.Decl.Instance, c, _) => {
            val itfName = getIdent(c).getOrElse("")
            val typName = nextId()
            val name = s"$itfName[$typName]"
            obj = (name, JObject(c.flatMap(inner).toList)) :: obj
          }
          case Tree(SyntaxTree.TreeKind.Decl.Def, c, _) => obj = innerSig(c) :: obj
          case Tree(SyntaxTree.TreeKind.Decl.Redef, c, _) => obj = innerSig(c) :: obj
          case Tree(SyntaxTree.TreeKind.Type.Effect, children, _) =>
            count += 1
            children.foreach(innerInner)
          case Tree(_, children, _) => children.foreach(innerInner)
          case Token(_, _, _, _, _, _) => ()
        }
      }
      innerInner(t)
      obj
    }

    def innerSig(s: Array[Child]): JField = {
      var id: String = ""

      var typeCount = 0
      var effectCount = 0

      def counter(children: Array[Child]): Unit = {
        children.foreach({
          case Tree(SyntaxTree.TreeKind.Type.Effect, c, _) =>
            effectCount += 1
            counter(c)
          case Token(TokenKind.Colon, _, _, _, _, _) => typeCount += 1
          case Tree(_, c, _) => counter(c)
          case Token(_, _, _, _, _, _) => ()
        })
      }

      s.foreach({
        case Tree(SyntaxTree.TreeKind.Ident, c, _) =>
          id = getIdent(c).getOrElse("")

        case _ => ()
      })
      counter(s)

      val tCount = JField("types", JInt(typeCount))
      val eCount = JField("effects", JInt(effectCount))
      JField(id, JObject(tCount::eCount::Nil))
    }

    def getIdent(s: Array[Child]): Option[String] = {
      s.foreach({
        case t @ Token(NameLowerCase, _, _, _, _, _) => return Some(t.text)
        case t @ Token(NameUpperCase, _, _, _, _, _) => return Some(t.text)
        case t @ Token(UserDefinedOperator, _, _, _, _, _) => return Some(t.text)
        case t @ Token(NameGreek, _, _, _, _, _) => return Some(t.text)
        case Tree(SyntaxTree.TreeKind.QName, c, _) => getIdent(c) match {
          case None => ()
          case res => return res
        }
        case Tree(SyntaxTree.TreeKind.Ident, c, _) => getIdent(c) match {
          case None => ()
          case res => return res
        }
        case _ => ()
      })
      None
    }
    inner(t)
  }

  private def printTree(t: Child): Unit = {
    val spacer = " "
    def inner(t: Child, depth: Int): Unit = {
      val dRep = if (depth < 10) s"$depth " else s"$depth"
      t match {
        case Tree(k, c, _) =>
          Console.println(s"  ${spacer.repeat(depth+1)}$k")
          c.foreach(inner(_, depth + 1))
        case Token(k,_,_,_,_,_) => Console.println(s"$dRep${spacer.repeat(depth+1)}$k")
      }
    }
    inner(t, 0)
  }
}
