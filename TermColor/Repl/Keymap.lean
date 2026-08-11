/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import TermColor.Widgets

/-!
# TermColor.Repl.Keymap

Inspectable, ordered keyboard bindings. Contexts are values rather than callbacks, so a keymap is
plain data: an application supplies the active context values for its current model.
-/

namespace TermColor.Repl

open TermColor
open TermColor.Widgets

/-- A validated name for an active keybinding context. -/
structure KeyContext where
  name : String
deriving Repr, BEq, DecidableEq

namespace KeyContext

/-- Construct an application context from one centralized name. -/
def ofString (name : String) : KeyContext := { name }

def editor : KeyContext := ofString "editor"
def completion : KeyContext := ofString "completion"
def multiline : KeyContext := ofString "multiline"

end KeyContext

structure KeyBinding (Action : Type) where
  key : Key
  action : Action
  context : Option KeyContext := none
  label : String := ""
  description : String := ""
deriving Repr

structure Keymap (Action : Type) where
  bindings : List (KeyBinding Action) := []
deriving Repr

/-- One declarative action binding. Multiple physical keys may name one action. -/
structure BindingSpec (Action : Type) where
  keys : List Key
  action : Action
  context : Option KeyContext := none
  label : String := ""
  description : String := ""
deriving Repr

namespace BindingSpec

variable {Action : Type}

def expand (spec : BindingSpec Action) : List (KeyBinding Action) :=
  spec.keys.map fun key =>
    { key, action := spec.action, context := spec.context
      label := spec.label, description := spec.description }

end BindingSpec

def Keymap.fromSpecs {Action : Type} (specs : List (BindingSpec Action)) : Keymap Action where
  bindings := specs.flatMap BindingSpec.expand

namespace Keymap

variable {Action : Type}

private def active (contexts : List KeyContext) : Option KeyContext → Bool
  | none => true
  | some context => contexts.any (· == context)

/-- First matching binding wins; list order is the precedence contract. -/
def resolveBinding (keymap : Keymap Action) (contexts : List KeyContext) (key : Key) :
    Option (KeyBinding Action) :=
  keymap.bindings.find? (fun binding => binding.key == key && active contexts binding.context)

/-- Resolve the first matching action. -/
def resolve (keymap : Keymap Action) (contexts : List KeyContext) (key : Key) : Option Action :=
  keymap.resolveBinding contexts key |>.map (·.action)

def visible (keymap : Keymap Action) (contexts : List KeyContext) : List (KeyBinding Action) :=
  keymap.bindings.filter (fun binding => active contexts binding.context)

def keys (keymap : Keymap Action) : List Key :=
  keymap.bindings.map (·.key)

/-- Return duplicate key/context pairs that rely on implicit first-match precedence. -/
def conflicts (keymap : Keymap Action) : List (Key × Option KeyContext) :=
  let step := fun (state : List (KeyBinding Action) × List (Key × Option KeyContext))
      (binding : KeyBinding Action) =>
    let (seen, conflicts) := state
    let duplicate := seen.any fun prior =>
      prior.key == binding.key && prior.context == binding.context
    if duplicate then
      let conflict := (binding.key, binding.context)
      if conflicts.any (· == conflict) then (binding :: seen, conflicts)
      else (binding :: seen, conflicts ++ [conflict])
    else (binding :: seen, conflicts)
  (keymap.bindings.foldl step ([], [])).2

def keyLabel : Key → String
  | .char value => s!"{value}"
  | .ctrl value => s!"Ctrl-{value}"
  | .left => "←"
  | .right => "→"
  | .home => "Home"
  | .end => "End"
  | .up => "↑"
  | .down => "↓"
  | .pageUp => "PgUp"
  | .pageDown => "PgDn"
  | .enter => "Enter"
  | .backspace => "Backspace"
  | .delete => "Delete"
  | .tab => "Tab"
  | .shiftTab => "Shift-Tab"
  | .escape => "Esc"

end Keymap

/-! ## Built-in editor bindings -/

inductive EditorAction where
  | complete
  | completionNext
  | completionPrevious
  | dismissCompletion
  | historyPrevious
  | historyNext
  | lineBreak
  | submit
  | quit
  | forceQuit
deriving Repr, BEq, DecidableEq

structure EditorContext where
  completionOpen : Bool := false
  multiline : Bool := false
deriving Repr, BEq, DecidableEq

def editorContexts (context : EditorContext) : List KeyContext :=
  let contexts := [KeyContext.editor]
  let contexts := if context.completionOpen then KeyContext.completion :: contexts else contexts
  if context.multiline then KeyContext.multiline :: contexts else contexts

private def binding (keys : List Key) (action : EditorAction) (context : Option KeyContext)
    (label description : String) : BindingSpec EditorAction :=
  { keys, action, context, label, description }

def defaultEditorKeymap (lineBreak : Key := .ctrl 'n') : Keymap EditorAction :=
  Keymap.fromSpecs
    [ binding [lineBreak] .lineBreak (some KeyContext.multiline) (Keymap.keyLabel lineBreak)
        "insert a line break"
    , binding [.up] .completionPrevious (some KeyContext.completion) (Keymap.keyLabel .up)
        "previous completion"
    , binding [.down] .completionNext (some KeyContext.completion) (Keymap.keyLabel .down)
        "next completion"
    , binding [.tab] .completionNext (some KeyContext.completion) (Keymap.keyLabel .tab)
        "next completion"
    , binding [.tab] .complete (some KeyContext.editor) (Keymap.keyLabel .tab) "complete input"
    , binding [.up] .historyPrevious (some KeyContext.editor) (Keymap.keyLabel .up)
        "previous history entry"
    , binding [.down] .historyNext (some KeyContext.editor) (Keymap.keyLabel .down)
        "next history entry"
    , binding [.enter] .submit (some KeyContext.editor) (Keymap.keyLabel .enter) "submit input"
    , binding [.escape] .dismissCompletion (some KeyContext.completion) (Keymap.keyLabel .escape)
        "close completion menu"
    , binding [.escape] .quit (some KeyContext.editor) (Keymap.keyLabel .escape) "quit"
    , binding [.ctrl 'x'] .forceQuit (some KeyContext.editor) (Keymap.keyLabel (.ctrl 'x')) "quit"
    ]

end TermColor.Repl
