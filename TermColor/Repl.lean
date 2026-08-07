/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import TermColor.Widgets

/-!
# TermColor.Repl

Pure input history, key handling, and adaptive completion for terminal REPLs.
The terminal loop is intentionally separate so applications can keep ownership
of their model and transcript while reusing this state machine.
-/

namespace TermColor.Repl

open TermColor
open TermColor.Layout
open TermColor.Widgets

structure Completion where
  replacement : String
  label : String := replacement
deriving Repr, BEq, DecidableEq

structure State where
  input : TextInputState := {}
  history : Array String := #[]
  historyIndex : Option Nat := none
deriving Repr

structure MultilineConfig where
  text : TextInputConfig := {}
  lineBreak : Key := .ctrl 'n'
deriving Repr

inductive Action where
  | changed
  | submit (line : String)
  | quit
deriving Repr, BEq, DecidableEq

private def inputState (value : String) : TextInputState :=
  { value, cursor := value.toList.length }

private def firstNewlineFrom : List Char → Nat → Option Nat
  | [], _ => none
  | character :: rest, index =>
      if character == '\n' then some index else firstNewlineFrom rest (index + 1)

private def lastNewlineBefore (chars : List Char) (position : Nat) : Nat :=
  let rec go : List Char → Nat → Nat → Nat
    | [], _, last => last
    | character :: rest, index, last =>
        if index == position then last
        else go rest (index + 1) (if character == '\n' then index + 1 else last)
  go chars 0 0

private def moveVertical (up : Bool) (state : TextInputState) : TextInputState :=
  let chars := state.value.toList
  let cursor := min state.cursor chars.length
  let start := lastNewlineBefore chars cursor
  let column := cursor - start
  if up then
    if start == 0 then state
    else
      let previousEnd := start - 1
      let previousStart := lastNewlineBefore chars previousEnd
      { state with cursor := min previousEnd (previousStart + column) }
  else
    match firstNewlineFrom (chars.drop start) start with
    | none => state
    | some lineEnd =>
        let nextStart := lineEnd + 1
        let nextEnd := (firstNewlineFrom (chars.drop nextStart) nextStart).getD chars.length
        { state with cursor := min nextEnd (nextStart + column) }

private def hasNewline (state : TextInputState) : Bool :=
  state.value.toList.any (· == '\n')

private def updateMultilineInput (config : MultilineConfig) (key : Key)
    (state : TextInputState) : TextInputState :=
  if key == config.lineBreak then
    let cursor := min state.cursor state.value.toList.length
    if state.value.toList.length < config.text.maxLength then
      let chars := state.value.toList
      { value := String.ofList (chars.take cursor ++ ['\n'] ++ chars.drop cursor)
        cursor := cursor + 1 }
    else
      { state with cursor }
  else
    updateTextInput config.text key state

private def commonPrefix : List Char → List Char → List Char
  | left :: rest, right :: tail =>
      if left == right then left :: commonPrefix rest tail else []
  | _, _ => []

private def sharedPrefix : List Completion → String
  | [] => ""
  | candidate :: rest =>
      String.ofList <| rest.foldl
        (fun shared next => commonPrefix shared next.replacement.toList)
        candidate.replacement.toList

def completeInput (input : TextInputState) (candidates : List Completion) : TextInputState :=
  match candidates with
  | [] => input
  | [candidate] => inputState candidate.replacement
  | candidates =>
      let shared := sharedPrefix candidates
      if shared.length > input.value.length then inputState shared else input

def renderMultilineTextInputBody (config : TextInputConfig) (state : TextInputState)
    (focused : Bool := false) : Text :=
  if !hasNewline state then
    textInputBody config state focused
  else
    let chars := state.value.toList
    let cursor := min state.cursor chars.length
    let before := String.ofList (chars.take cursor)
    let cursorCharacter := (chars.drop cursor).head?.getD ' '
    let after := String.ofList (chars.drop (if focused then cursor + 1 else cursor))
    let cursorText := if focused then
        let style := Style.combine config.textStyle config.cursorStyle
        Text.styled (String.singleton cursorCharacter) style
      else Text.empty
    let content := wrapLines (max 1 config.width)
      (Text.styled before config.textStyle ++ cursorText ++ Text.styled after config.textStyle)
    align (max 1 config.width) .left content

def recallUp (state : State) : State :=
  if state.history.isEmpty then state
  else
    let index := match state.historyIndex with
      | none => state.history.size - 1
      | some index => index.pred
    { state with
      input := inputState (state.history.getD index "")
      historyIndex := some index }

def recallDown (state : State) : State :=
  match state.historyIndex with
  | none => state
  | some index =>
      if index + 1 < state.history.size then
        let next := index + 1
        { state with
          input := inputState (state.history.getD next "")
          historyIndex := some next }
      else
        { state with input := {}, historyIndex := none }

def updateMultiline (config : MultilineConfig) (complete : TextInputState → List Completion)
    (state : State) (key : Key) : State × Action :=
  match key with
  | .up =>
      if hasNewline state.input then
        ({ state with input := moveVertical true state.input }, .changed)
      else
        (recallUp state, .changed)
  | .down =>
      if hasNewline state.input then
        ({ state with input := moveVertical false state.input }, .changed)
      else
        (recallDown state, .changed)
  | .tab =>
      ({ state with
        input := completeInput state.input (complete state.input)
        historyIndex := none },
        .changed)
  | .enter =>
      let line := state.input.value.trimAscii.toString
      if line.isEmpty then
        ({ state with input := {}, historyIndex := none }, .changed)
      else
        ({ state with
          input := {}
          history := state.history.push line
          historyIndex := none }, .submit line)
  | .escape => (state, .quit)
  | key =>
      ({ state with
        input := updateMultilineInput config key state.input
        historyIndex := none }, .changed)

def update (config : TextInputConfig) (complete : TextInputState → List Completion)
    (state : State) (key : Key) : State × Action :=
  match key with
  | .up => (recallUp state, .changed)
  | .down => (recallDown state, .changed)
  | .tab =>
      ({ state with
        input := completeInput state.input (complete state.input)
        historyIndex := none },
        .changed)
  | .enter =>
      let line := state.input.value.trimAscii.toString
      if line.isEmpty then
        ({ state with input := {}, historyIndex := none }, .changed)
      else
        ({ state with
          input := {}
          history := state.history.push line
          historyIndex := none }, .submit line)
  | .escape => (state, .quit)
  | key =>
      ({ state with
        input := updateTextInput config key state.input
        historyIndex := none }, .changed)

end TermColor.Repl
