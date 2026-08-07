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

structure Cancellation where
  flag : IO.Ref Bool

namespace Cancellation

def new : IO Cancellation := do
  pure { flag := ← IO.mkRef false }

def cancel (token : Cancellation) : IO Unit :=
  token.flag.set true

def isCancelled (token : Cancellation) : IO Bool :=
  token.flag.get

def sleep (token : Cancellation) (milliseconds : UInt32) : IO Bool := do
  if ← token.isCancelled then
    pure false
  else
    IO.sleep milliseconds
    pure !(← token.isCancelled)

end Cancellation

structure JobConfig (Model : Type) where
  shouldRun : Model → String → Bool := fun _ _ => true
  start : Model → String → Model
  run : Cancellation → Screen → Model → String → IO (Screen × Model)
  finish : Model → Model → Model := fun _ completed => completed
  cancel : Model → Model := id
  fail : Model → String → Model := fun model _ => model

structure Config (Model : Type) where
  initial : Model
  inputConfig : TextInputConfig
  multiline : Option MultilineConfig := none
  fallbackSize : Size := { columns := 80, rows := 24 }
  tickMs : UInt32 := 60
  view : Model → Size → Text
  complete : Model → TextInputState → IO (List Completion)
  getState : Model → State
  setState : Model → State → Model
  submit : Screen → Model → String → IO (Screen × Model)
  jobs : Option (JobConfig Model) := none
  isRunning : Model → Bool
  quit : Model → Model

def currentSize (fallback : Size) : IO Size := do
  pure ((← terminalSize).getD fallback)

def readKeyWithResize (tickMs : UInt32) (fallback : Size) (screen : Screen)
    (render : Screen → IO Screen) (wake : IO Bool := pure false) : IO (Screen × Option Key) := do
  let result ← IO.mkRef (none : Option (Option Key))
  let _task ← IO.asTask do
    result.set (some (← readKey))
  let mut screen := screen
  let mut size ← currentSize fallback
  while (← result.get).isNone do
    if ← wake then
      result.set (some none)
    else
      let nextSize ← currentSize fallback
      if nextSize != size then
        screen ← render screen
        size := nextSize
      IO.sleep tickMs
  pure (screen, (← result.get).getD none)

private structure JobRuntime (Model : Type) where
  cancellation : Cancellation
  result : IO.Ref (Option (Except String (Screen × Model)))

private def render {Model : Type} (config : Config Model) (screen : Screen)
    (model : Model) : IO Screen := do
  screen.render (config.view model (← currentSize config.fallbackSize))

def run {Model : Type} (config : Config Model) : IO Unit := do
  hideCursor
  try
    withRawInput do
      let mut model := config.initial
      let mut screen ← Screen.start
      let mut job : Option (JobRuntime Model) := none
      while config.isRunning model do
        match config.jobs, job with
        | some jobs, some runtime =>
            match ← runtime.result.get with
            | none => pure ()
            | some (.ok (nextScreen, nextModel)) =>
                model := jobs.finish model nextModel
                screen := nextScreen
                job := none
            | some (.error message) =>
                model := jobs.fail model message
                job := none
        | _, _ => pure ()
        screen ← render config screen model
        let wake : IO Bool := match job with
          | some runtime => do pure (← runtime.result.get).isSome
          | none => pure false
        let (nextScreen, key) ← readKeyWithResize config.tickMs config.fallbackSize screen
          (fun screen => render config screen model) wake
        screen := nextScreen
        match key with
        | none =>
            if job.isNone then
              model := config.quit model
        | some key =>
            if key == .escape then
              match config.jobs, job with
              | some jobs, some runtime =>
                  runtime.cancellation.cancel
                  model := jobs.cancel model
                  job := none
              | _, _ =>
                  let currentState := config.getState model
                  let (state, action) := match config.multiline with
                    | some multiline => TermColor.Repl.updateMultiline multiline (fun _ => [])
                        currentState key
                    | none => TermColor.Repl.update config.inputConfig (fun _ => [])
                        currentState key
                  model := config.setState model state
                  if action == .quit then
                    model := config.quit model
            else
              let currentState := config.getState model
              let candidates ← if key == .tab && currentState.completion.isNone then
                  config.complete model currentState.input
                else
                  pure []
              let (state, action) := match config.multiline with
                | some multiline => TermColor.Repl.updateMultiline multiline
                    (fun _ => candidates) currentState key
                | none => TermColor.Repl.update config.inputConfig
                    (fun _ => candidates) currentState key
              match job, action with
              | some _, .submit _ => pure ()
              | _, _ =>
                  model := config.setState model state
                  match action with
                  | .changed => pure ()
                  | .quit => model := config.quit model
                  | .submit line =>
                      match config.jobs with
                      | some jobs =>
                          if jobs.shouldRun model line then
                            let cancellation ← Cancellation.new
                            let result ← IO.mkRef none
                            let started := jobs.start model line
                            let runtime : JobRuntime Model := { cancellation, result }
                            let _task ← IO.asTask do
                              try
                                result.set (some (.ok
                                  (← jobs.run cancellation screen started line)))
                              catch error =>
                                result.set (some (.error error.toString))
                            model := started
                            job := some runtime
                          else
                            let (nextScreen, nextModel) ← config.submit screen model line
                            screen := nextScreen
                            model := nextModel
                      | none =>
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
