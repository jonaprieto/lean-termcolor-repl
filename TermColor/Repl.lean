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

inductive Action where
  | changed
  | submit (line : String)
  | quit
deriving Repr, BEq, DecidableEq

private def inputState (value : String) : TextInputState :=
  { value, cursor := value.toList.length }

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
