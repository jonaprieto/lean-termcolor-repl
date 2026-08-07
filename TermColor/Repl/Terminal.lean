/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import TermColor.Repl
import TermColor.Terminal
import Std.Async.Timer
import Std.Sync.CancellationToken
import Std.Sync.Notify

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
  token : Std.CancellationToken

namespace Cancellation

def new : IO Cancellation := do
  pure { token := ← Std.CancellationToken.new }

def cancel (token : Cancellation) : IO Unit :=
  token.token.cancel

def isCancelled (token : Cancellation) : IO Bool :=
  token.token.isCancelled

def sleep (token : Cancellation) (milliseconds : UInt32) : IO Bool := do
  if ← token.isCancelled then
    pure false
  else
    let timer ← (Std.Async.Selector.sleep
      (Std.Time.Millisecond.Offset.ofNat milliseconds.toNat)).block
    let completed ← (Std.Async.Selectable.one #[
      .case token.token.selector (fun _ => pure false),
      .case timer (fun _ => pure true)
    ]).block
    if completed then
      pure !(← token.isCancelled)
    else
      pure false

end Cancellation

structure KeyReader where
  result : IO.Ref (Option (Option Key))
  active : IO.Ref Bool
  signal : Std.Notify

namespace KeyReader

def new : IO KeyReader := do
  pure {
    result := ← IO.mkRef none
    active := ← IO.mkRef false
    signal := ← Std.Notify.new
  }

def ensureReading (reader : KeyReader) : IO Unit := do
  unless ← reader.active.get do
    reader.active.set true
    let _task ← IO.asTask do
      try
        reader.result.set (some (← readKey))
      finally
        reader.active.set false
        reader.signal.notify

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
  resizeMs : UInt32 := 250
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

private def waitForEvent (tickMs : UInt32) (reader : KeyReader)
    (wakeSignal : Option Std.Notify) : IO Bool := do
  match wakeSignal with
  | none =>
      IO.sleep tickMs
      pure true
  | some wakeSignal =>
      let timer ← (Std.Async.Selector.sleep
        (Std.Time.Millisecond.Offset.ofNat tickMs.toNat)).block
      (Std.Async.Selectable.one #[
        .case reader.signal.selector (fun _ => pure false),
        .case wakeSignal.selector (fun _ => pure false),
        .case timer (fun _ => pure true)
      ]).block

private def readKeyWithResizeAtSize (tickMs : UInt32) (fallback : Size) (screen : Screen)
    (render : Screen → Size → IO Screen) (wake : IO Bool := pure false)
    (reader : Option KeyReader := none) (wakeSignal : Option Std.Notify := none) :
    IO (Screen × Option Key) := do
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
      if ← waitForEvent tickMs reader wakeSignal then
        let nextSize ← currentSize fallback
        if nextSize != size then
          screen ← render screen nextSize
          size := nextSize
  if woken then
    pure (screen, none)
  else
    let key := (← reader.result.get).getD none
    reader.result.set none
    reader.active.set false
    pure (screen, key)

def readKeyWithResize (tickMs : UInt32) (fallback : Size) (screen : Screen)
    (render : Screen → IO Screen) (wake : IO Bool := pure false)
    (reader : Option KeyReader := none) (wakeSignal : Option Std.Notify := none) :
    IO (Screen × Option Key) :=
  readKeyWithResizeAtSize tickMs fallback screen (fun screen _ => render screen)
    wake reader wakeSignal

private structure JobRuntime (Model : Type) where
  cancellation : Cancellation
  result : IO.Ref (Option (Except String Model))

private def renderAtSize {Model : Type} (config : Config Model) (screen : Screen)
    (model : Model) (size : Size) : IO Screen :=
  screen.render (config.view model size)

private def render {Model : Type} (config : Config Model) (screen : Screen)
    (model : Model) : IO Screen := do
  renderAtSize config screen model (← currentSize config.fallbackSize)

def run {Model : Type} (config : Config Model) : IO Unit := do
  hideCursor
  try
    withRawInput do
      let mut model := config.initial
      let mut screen ← Screen.start
      let mut activeJobs : List (JobRuntime Model) := []
      let reader ← KeyReader.new
      let wakeSignal ← Std.Notify.new
      let mut dirty := true
      while config.isRunning model do
        let mut pendingJobs : List (JobRuntime Model) := []
        for runtime in activeJobs do
          match ← runtime.result.get with
          | none => pendingJobs := runtime :: pendingJobs
          | some (.ok nextModel) =>
              match config.jobs with
              | some jobs =>
                  model := jobs.finish model nextModel
                  dirty := true
              | none => pure ()
          | some (.error message) =>
              match config.jobs with
              | some jobs =>
                  model := jobs.fail model message
                  dirty := true
              | none => pure ()
        activeJobs := pendingJobs.reverse
        if dirty then
          screen ← render config screen model
          dirty := false
        let wake : IO Bool := do
          for runtime in activeJobs do
            if (← runtime.result.get).isSome then
              return true
          pure false
        let (nextScreen, key) ← readKeyWithResizeAtSize config.resizeMs config.fallbackSize screen
          (fun screen size => renderAtSize config screen model size) wake (some reader)
          (some wakeSignal)
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
                  dirty := true
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
                    dirty := true
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
              dirty := true
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
                          finally
                            wakeSignal.notify
                        model := started
                        activeJobs := runtime :: activeJobs
                        dirty := true
                      else
                        model ← config.submit model line
                        dirty := true
                  | none =>
                      model ← config.submit model line
                      dirty := true
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
