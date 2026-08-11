/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import TermColor.Widgets
import TermColor.Repl.Keymap

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

inductive CompletionKind where
  | text
  | file
  | directory
  | executable
deriving Repr, BEq, DecidableEq

structure Completion where
  replacement : String
  label : String := replacement
  kind : CompletionKind := .text
  range : Option (Nat × Nat) := none
deriving Repr, BEq, DecidableEq

structure CompletionMenu where
  candidates : Array Completion
  selected : Nat := 0
deriving Repr, BEq, DecidableEq

structure CompletionMenuConfig where
  width : Nat := 80
  maxItems : Nat := 8
  selectedStyle : Style := Style.reverse
  textStyle : Style := {}
  kindStyle : Style := Style.dim
deriving Repr

structure State where
  input : TextInputState := {}
  history : Array String := #[]
  historyIndex : Option Nat := none
  completion : Option CompletionMenu := none
deriving Repr

structure MultilineConfig where
  text : TextInputConfig := {}
  lineBreak : Key := .ctrl 'n'
  keymap : Option (Keymap EditorAction) := none
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
  updateTextInput config.text key state

private def insertLineBreak (config : MultilineConfig) (state : TextInputState) : TextInputState :=
  let cursor := min state.cursor state.value.toList.length
  if state.value.toList.length < config.text.maxLength then
    let chars := state.value.toList
    { value := String.ofList (chars.take cursor ++ ['\n'] ++ chars.drop cursor)
      cursor := cursor + 1 }
  else
    { state with cursor }

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

private def applyCompletion (input : TextInputState) (candidate : Completion)
    (replacement : String := candidate.replacement) : TextInputState :=
  let chars := input.value.toList
  let (rawStart, rawStop) := candidate.range.getD (0, chars.length)
  let start := min rawStart chars.length
  let stop := max start (min rawStop chars.length)
  let value := String.ofList
    (chars.take start ++ replacement.toList ++ chars.drop stop)
  { value, cursor := start + replacement.toList.length }

private def completionRange (input : TextInputState) (candidate : Completion) : Nat × Nat :=
  let length := input.value.toList.length
  let (rawStart, rawStop) := candidate.range.getD (0, length)
  (min rawStart length, max (min rawStart length) (min rawStop length))

def completeInput (input : TextInputState) (candidates : List Completion) : TextInputState :=
  match candidates with
  | [] => input
  | [candidate] => applyCompletion input candidate
  | candidate :: rest =>
      let candidates := candidate :: rest
      let shared := sharedPrefix candidates
      let (start, stop) := completionRange input candidate
      let current := String.ofList (input.value.toList.drop start |>.take (stop - start))
      if shared.length > current.length then applyCompletion input candidate shared else input

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

private def completionKindLabel : CompletionKind → String
  | .text => "  "
  | .file => "ƒ "
  | .directory => "▸ "
  | .executable => "⚙ "

/-- Render a width-bounded completion menu with a selected row and kind markers. -/
def renderCompletionMenu (config : CompletionMenuConfig) (menu : CompletionMenu) : Text :=
  let width := max 1 config.width
  let count := min config.maxItems menu.candidates.size
  let rows := (List.range count).filterMap fun index => do
    let candidate ← menu.candidates[index]?
    let marker := if index == menu.selected then "› " else "  "
    let row := marker ++ completionKindLabel candidate.kind ++ candidate.label
    let style := if index == menu.selected then config.selectedStyle else config.textStyle
    pure (truncate width (Text.styled row style))
  let more := if menu.candidates.size > count then
      [Text.styled s!"  … {menu.candidates.size - count} more" config.kindStyle]
    else []
  align width .left (joinLines (rows ++ more))

private def selectCompletion (state : State) (selected : Nat) : State :=
  match state.completion with
  | none => state
  | some menu =>
      match menu.candidates[selected]? with
      | none => state
      | some candidate =>
          { state with
            input := applyCompletion state.input candidate
            completion := some { menu with selected } }

private def cycleCompletion (state : State) (forward : Bool) : State :=
  match state.completion with
  | some menu =>
      if menu.candidates.isEmpty then state
      else
        let selected := if forward then
            (menu.selected + 1) % menu.candidates.size
          else if menu.selected == 0 then menu.candidates.size - 1 else menu.selected - 1
        selectCompletion state selected
  | none => state

private def completeWith (state : State) (candidates : List Completion) : State :=
  match candidates with
  | [] => { state with completion := none }
  | [_] => { state with
      input := completeInput state.input candidates
      historyIndex := none
      completion := none }
  | _ => { state with
      input := completeInput state.input candidates
      historyIndex := none
      completion := some { candidates := candidates.toArray } }

private def acceptCompletion (state : State) : State :=
  match state.completion with
  | none => state
  | some menu => selectCompletion state menu.selected

private def updateCommon (keymap : Keymap EditorAction) (multiline : Bool)
    (complete : TextInputState → List Completion)
    (up down : State → State)
    (inputUpdate : Key → TextInputState → TextInputState)
    (lineBreak : TextInputState → TextInputState)
    (state : State) (key : Key) : State × Action :=
  let context := { completionOpen := state.completion.isSome, multiline }
  match keymap.resolve (editorContexts context) key with
  | some .completionPrevious =>
      (cycleCompletion state false, .changed)
  | some .completionNext =>
      (cycleCompletion state true, .changed)
  | some .complete =>
      (completeWith state (complete state.input), .changed)
  | some .dismissCompletion =>
      ({ state with completion := none }, .changed)
  | some .historyPrevious =>
      (up state, .changed)
  | some .historyNext =>
      (down state, .changed)
  | some .lineBreak =>
      ({ state with input := lineBreak state.input }, .changed)
  | some .submit =>
      let state := { acceptCompletion state with completion := none }
      let line := state.input.value.trimAscii.toString
      if line.isEmpty then
        ({ state with input := {}, historyIndex := none }, .changed)
      else
        ({ state with
          input := {}
          history := state.history.push line
          historyIndex := none }, .submit line)
  | some .quit =>
      (state, .quit)
  | some .forceQuit =>
      (state, .quit)
  | none =>
      ({ state with
        input := inputUpdate key state.input
        historyIndex := none
        completion := none }, .changed)

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
  updateCommon (config.keymap.getD (defaultEditorKeymap config.lineBreak)) true complete
    (fun state =>
      if hasNewline state.input then
        { state with input := moveVertical true state.input }
      else recallUp state)
    (fun state =>
      if hasNewline state.input then
        { state with input := moveVertical false state.input }
      else recallDown state)
    (fun key input => updateMultilineInput config key input)
    (insertLineBreak config) state key

def update (config : TextInputConfig) (complete : TextInputState → List Completion)
    (state : State) (key : Key) : State × Action :=
  updateCommon (defaultEditorKeymap) false complete recallUp recallDown
    (fun key input => updateTextInput config key input) id state key

end TermColor.Repl
