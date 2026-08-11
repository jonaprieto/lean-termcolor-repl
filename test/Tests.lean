/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/

import TermColor.Repl.Command

open Argus
open TermColor
open TermColor.Repl
open TermColor.Widgets

private def check (name : String) (condition : Bool) : Option String :=
  if condition then none else some name

private def nestedCommand : Argus.Command Unit :=
  Argus.group "tool"
    [Argus.group "parent" [Argus.cmd "child" (Spec.const ())]]

private def names (completions : List Completion) : List String :=
  completions.map (·.replacement)

def main (_argv : List String) : IO UInt32 := do
  let nested ← completeCommand nestedCommand { value := "/parent ", cursor := 8 }
  let top ← completeCommand nestedCommand { value := "/pa", cursor := 3 }
  let parsed := match parseCommand nestedCommand "/parent child" with
    | .ok () => true
    | .error _ => false
  let checks :=
    [check "nested command completion" (names nested == ["child"])
    , check "top-level command completion" (names top == ["/parent"])
    , check "nested command parsing" parsed]
  let failures := checks.filterMap id
  if failures.isEmpty then
    IO.println "all REPL command checks passed"
    pure 0
  else
    for failure in failures do
      IO.println s!"FAIL: {failure}"
    pure 1
