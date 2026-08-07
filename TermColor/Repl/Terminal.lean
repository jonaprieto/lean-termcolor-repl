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

structure KeyReader where
  result : IO.Ref (Option (Option Key))
  active : IO.Ref Bool

namespace KeyReader

def new : IO KeyReader := do
  pure { result := ← IO.mkRef none, active := ← IO.mkRef false }

def ensureReading (reader : KeyReader) : IO Unit := do
  unless ← reader.active.get do
    reader.active.set true
    let _task ← IO.asTask do
      try
        reader.result.set (some (← readKey))
      finally
        reader.active.set false

end KeyReader

/-- Configuration for cooperative background jobs.

Multiple submitted lines may run at the same time. The renderer remains the
owner of `Model` and `Screen`; each worker returns only model state which is
merged through `finish` when its result is drained.
-/
structure JobConfig (Model : Type) where
  shouldRun : Model → String → Bool := fun _ _ => true
  start : Model → String → Model
  run : Cancellation → Model → String → IO Model
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
  submit : Model → String → IO Model
  jobs : Option (JobConfig Model) := none
  isRunning : Model → Bool
  quit : Model → Model

def currentSize (fallback : Size) : IO Size := do
  pure ((← terminalSize).getD fallback)

def readKeyWithResize (tickMs : UInt32) (fallback : Size) (screen : Screen)
    (render : Screen → IO Screen) (wake : IO Bool := pure false)
    (reader : Option KeyReader := none) : IO (Screen × Option Key) := do
  let reader ← match reader with
    | some reader => pure reader
    | none => KeyReader.new
  reader.ensureReading
  let mut screen := screen
  let mut size ← currentSize fallback
  let mut woken := false
  while !woken && (← reader.result.get).isNone do
    if ← wake then
      woken := true
    else
      let nextSize ← currentSize fallback
      if nextSize != size then
        screen ← render screen
        size := nextSize
      IO.sleep tickMs
  if woken then
    pure (screen, none)
  else
    let key := (← reader.result.get).getD none
    reader.result.set none
    reader.active.set false
    pure (screen, key)

private structure JobRuntime (Model : Type) where
  cancellation : Cancellation
  result : IO.Ref (Option (Except String Model))

private def render {Model : Type} (config : Config Model) (screen : Screen)
    (model : Model) : IO Screen := do
  screen.render (config.view model (← currentSize config.fallbackSize))

def run {Model : Type} (config : Config Model) : IO Unit := do
  hideCursor
  try
    withRawInput do
      let mut model := config.initial
      let mut screen ← Screen.start
      let mut activeJobs : List (JobRuntime Model) := []
      let reader ← KeyReader.new
      while config.isRunning model do
        let mut pendingJobs : List (JobRuntime Model) := []
        for runtime in activeJobs do
          match ← runtime.result.get with
          | none => pendingJobs := runtime :: pendingJobs
          | some (.ok nextModel) =>
              match config.jobs with
              | some jobs =>
                  model := jobs.finish model nextModel
              | none => pure ()
          | some (.error message) =>
              match config.jobs with
              | some jobs => model := jobs.fail model message
              | none => pure ()
        activeJobs := pendingJobs.reverse
        screen ← render config screen model
        let wake : IO Bool := do
          for runtime in activeJobs do
            if (← runtime.result.get).isSome then
              return true
          pure false
        let (nextScreen, key) ← readKeyWithResize config.tickMs config.fallbackSize screen
          (fun screen => render config screen model) wake (some reader)
        screen := nextScreen
        match key with
        | none =>
            if activeJobs.isEmpty then
              model := config.quit model
        | some key =>
            if key == .escape || key == .ctrl 'x' then
              match config.jobs, activeJobs.isEmpty with
              | some jobs, false =>
                  for runtime in activeJobs do
                    runtime.cancellation.cancel
                  model := jobs.cancel model
                  activeJobs := []
              | _, _ =>
                  if key == .ctrl 'x' then
                    model := config.quit model
                  else
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
                              (← jobs.run cancellation started line)))
                          catch error =>
                            result.set (some (.error error.toString))
                        model := started
                        activeJobs := runtime :: activeJobs
                      else
                        model ← config.submit model line
                  | none =>
                      model ← config.submit model line
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
