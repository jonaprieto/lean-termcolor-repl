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

private def binding (key : Key) (action : EditorAction) (context : Option String)
    (label description : String) : KeyBinding EditorAction :=
  { key, action, context, label, description }

def defaultEditorKeymap (lineBreak : Key := .ctrl 'n') : Keymap EditorAction :=
  { bindings :=
    [ binding .up .completionPrevious (some "completion") "↑" "previous completion"
    , binding .down .completionNext (some "completion") "↓" "next completion"
    , binding .tab .completionNext (some "completion") "tab" "next completion"
    , binding .tab .complete (some "editor") "tab" "complete input"
    , binding .up .historyPrevious (some "editor") "↑" "previous history entry"
    , binding .down .historyNext (some "editor") "↓" "next history entry"
    , binding .enter .submit (some "editor") "enter" "submit input"
    , binding .escape .dismissCompletion (some "completion") "esc" "close completion menu"
    , binding .escape .quit (some "editor") "esc" "quit"
    , binding (.ctrl 'x') .forceQuit (some "editor") "ctrl-x" "quit"
    , binding lineBreak .lineBreak (some "multiline") "ctrl-n" "insert a line break"
    ] }

end TermColor.Repl
