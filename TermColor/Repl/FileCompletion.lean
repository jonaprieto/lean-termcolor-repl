/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import TermColor.Repl

/-!
# TermColor.Repl.FileCompletion

Filesystem completion for the token under the cursor.
-/

namespace TermColor.Repl

open TermColor.Widgets

structure FileCompletionConfig where
  maxCandidates : Nat := 64
  includeHidden : Bool := false
deriving Repr

private def whitespace (character : Char) : Bool :=
  character == ' ' || character == '\t' || character == '\n'

private def tokenStart (chars : List Char) (cursor : Nat) : Nat :=
  let rec go : List Char → Nat → Nat → Nat
    | [], _, start => start
    | character :: rest, index, start =>
        if index == cursor then start
        else go rest (index + 1) (if whitespace character then index + 1 else start)
  go chars 0 0

private def tokenRange (input : TextInputState) : Nat × Nat :=
  let chars := input.value.toList
  let cursor := min input.cursor chars.length
  let start := tokenStart chars cursor
  let stop := cursor +
    ((chars.drop cursor).takeWhile (fun character => !whitespace character)).length
  (start, stop)

private def safeReadDir (directory : System.FilePath) : IO (Array IO.FS.DirEntry) :=
  try directory.readDir catch _ => pure #[]

private def replacementPrefix (token : String) (parent : System.FilePath) : String :=
  let separator := System.FilePath.pathSeparator.toString
  if parent.toString == "." then
    if token.startsWith ("." ++ separator) then "." ++ separator else ""
  else parent.toString ++ separator

private def completionFor (token : String) (start stop : Nat)
    (entry : IO.FS.DirEntry) (directory : System.FilePath) : IO Completion := do
  let isDirectory ← entry.path.isDir.toIO
  pure {
    replacement := replacementPrefix token directory ++ entry.fileName ++
      if isDirectory then System.FilePath.pathSeparator.toString else ""
    label := entry.fileName ++ if isDirectory then System.FilePath.pathSeparator.toString else ""
    kind := if isDirectory then .directory else .file
    range := some (start, stop) }

def fileCompletions (config : FileCompletionConfig) (input : TextInputState) :
    IO (List Completion) := do
  if config.maxCandidates == 0 then
    return []
  let (start, stop) := tokenRange input
  let token := String.ofList (input.value.toList.drop start |>.take (stop - start))
  let path : System.FilePath := ⟨token⟩
  let directory := path.parent.getD ⟨"."⟩
  let fragment := path.fileName.getD ""
  let mut completions : List Completion := []
  for entry in ← safeReadDir directory do
    if (config.includeHidden || !entry.fileName.startsWith "." || fragment.startsWith ".") &&
        entry.fileName.startsWith fragment then
      completions := (← completionFor token start stop entry directory) :: completions
  pure ((completions.mergeSort (fun left right => left.label < right.label)).take
    config.maxCandidates)

def defaultFileCompletions (input : TextInputState) : IO (List Completion) :=
  fileCompletions {} input

end TermColor.Repl
