/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import TermColor.Repl
import TermColor.Terminal

/-!
# TermColor.Repl.Terminal

Small terminal primitives shared by REPL loops. Model state, rendering, and
submission remain callbacks supplied by the application.
-/

namespace TermColor.Repl.Terminal

open TermColor
open TermColor.Repl
open TermColor.Terminal
open TermColor.Widgets

structure Config (Model : Type) where
  initial : Model
  inputConfig : TextInputConfig
  fallbackSize : Size := { columns := 80, rows := 24 }
  tickMs : UInt32 := 60
  view : Model → Size → Text
  complete : Model → TextInputState → List Completion
  getState : Model → State
  setState : Model → State → Model
  submit : Screen → Model → String → IO (Screen × Model)
  isRunning : Model → Bool
  quit : Model → Model

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

private def render {Model : Type} (config : Config Model) (screen : Screen)
    (model : Model) : IO Screen := do
  screen.render (config.view model (← currentSize config.fallbackSize))

def run {Model : Type} (config : Config Model) : IO Unit := do
  hideCursor
  try
    withRawInput do
      let mut model := config.initial
      let mut screen ← Screen.start
      while config.isRunning model do
        screen ← render config screen model
        let (nextScreen, key) ← readKeyWithResize config.tickMs config.fallbackSize screen
          (fun screen => render config screen model)
        screen := nextScreen
        match key with
        | none => model := config.quit model
        | some key =>
            let (state, action) := TermColor.Repl.update config.inputConfig
              (config.complete model) (config.getState model) key
            model := config.setState model state
            match action with
            | .changed => pure ()
            | .quit => model := config.quit model
            | .submit line =>
                let (nextScreen, nextModel) ← config.submit screen model line
                screen := nextScreen
                model := nextModel
  finally
    showCursor
    clearScreen

def suspend {α : Type} (action : IO α) : IO (Screen × α) := do
  clearScreen
  try
    let result ← action
    pure (Screen.empty, result)
  finally
    clearScreen

end TermColor.Repl.Terminal
