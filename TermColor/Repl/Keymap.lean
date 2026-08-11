/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import TermColor.Widgets

/-!
# TermColor.Repl.Keymap

Inspectable, ordered keyboard bindings. Contexts are names rather than callbacks, so a keymap is
plain data: an application supplies the active context names for its current model.
-/

namespace TermColor.Repl

open TermColor
open TermColor.Widgets

structure KeyBinding (Action : Type) where
  key : Key
  action : Action
  context : Option String := none
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
  context : Option String := none
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

private def active (contexts : List String) : Option String → Bool
  | none => true
  | some context => context ∈ contexts

/-- First matching binding wins; list order is the precedence contract. -/
def resolve (keymap : Keymap Action) (contexts : List String) (key : Key) : Option Action :=
  keymap.bindings.find? (fun binding => binding.key == key && active contexts binding.context)
    |>.map (·.action)

def visible (keymap : Keymap Action) (contexts : List String) : List (KeyBinding Action) :=
  keymap.bindings.filter (fun binding => active contexts binding.context)

def keys (keymap : Keymap Action) : List Key :=
  keymap.bindings.map (·.key)

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

def editorContexts (context : EditorContext) : List String :=
  let contexts := ["editor"]
  let contexts := if context.completionOpen then "completion" :: contexts else contexts
  if context.multiline then "multiline" :: contexts else contexts

private def binding (keys : List Key) (action : EditorAction) (context : Option String)
    (label description : String) : BindingSpec EditorAction :=
  { keys, action, context, label, description }

def defaultEditorKeymap (lineBreak : Key := .ctrl 'n') : Keymap EditorAction :=
  Keymap.fromSpecs
    [ binding [lineBreak] .lineBreak (some "multiline") (Keymap.keyLabel lineBreak)
        "insert a line break"
    , binding [.up] .completionPrevious (some "completion") (Keymap.keyLabel .up) "previous completion"
    , binding [.down] .completionNext (some "completion") (Keymap.keyLabel .down) "next completion"
    , binding [.tab] .completionNext (some "completion") (Keymap.keyLabel .tab) "next completion"
    , binding [.tab] .complete (some "editor") (Keymap.keyLabel .tab) "complete input"
    , binding [.up] .historyPrevious (some "editor") (Keymap.keyLabel .up) "previous history entry"
    , binding [.down] .historyNext (some "editor") (Keymap.keyLabel .down) "next history entry"
    , binding [.enter] .submit (some "editor") (Keymap.keyLabel .enter) "submit input"
    , binding [.escape] .dismissCompletion (some "completion") (Keymap.keyLabel .escape) "close completion menu"
    , binding [.escape] .quit (some "editor") (Keymap.keyLabel .escape) "quit"
    , binding [.ctrl 'x'] .forceQuit (some "editor") (Keymap.keyLabel (.ctrl 'x')) "quit"
    ]

end TermColor.Repl
