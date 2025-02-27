/*
 * Copyright 2022 Paul Butcher, Lukas Rønn
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
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}

object TypeCompleter {

  /**
    * Returns a List of Completion for types (enums, aliases and builtin).
    */
  def getCompletions(context: CompletionContext)(implicit root: TypedAst.Root): Iterable[Completion] = {
    ModuleCompleter.getCompletions(context) ++
      TypeBuiltinCompleter.getCompletions
  }

  /**
    * Get the internal priority from the TypedAst SourceLocation and namespace
    */
  def getInternalPriority(loc: SourceLocation, ns: List[String])(implicit context: CompletionContext): Priority = {
    if (loc.source.name == context.uri)
      Priority.Higher
    else if (ns.isEmpty)
      Priority.Low
    else
      Priority.Lower
  }
}
