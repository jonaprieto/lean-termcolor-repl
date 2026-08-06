/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import TermColor.Terminal

/-!
# TermColor.Repl.Terminal

Small terminal primitives shared by REPL loops. Model state, rendering, and
submission remain callbacks supplied by the application.
-/

namespace TermColor.Repl.Terminal

open TermColor
open TermColor.Terminal
open TermColor.Widgets

def currentSize (fallback : Size) : IO Size := do
  pure ((← terminalSize).getD fallback)

def readKeyWithResize (tickMs : UInt32) (fallback : Size) (screen : Screen)
    (render : Screen → IO Screen) : IO (Screen × Option Key) := do
  let result ← IO.mkRef (none : Option (Option Key))
  let _task ← IO.asTask do
    result.set (some (← readKey))
  let mut screen := screen
  let mut size ← currentSize fallback
  while (← result.get).isNone do
    let nextSize ← currentSize fallback
    if nextSize != size then
      screen ← render screen
      size := nextSize
    IO.sleep tickMs
  pure (screen, (← result.get).getD none)

end TermColor.Repl.Terminal
