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

private structure EventReader where
  result : IO.Ref (Option (Option Event))
  active : IO.Ref Bool
  signal : Std.Notify

private def newEventReader : IO EventReader := do
  pure {
    result := ← IO.mkRef none
    active := ← IO.mkRef false
    signal := ← Std.Notify.new
  }

private def ensureEventReading (reader : EventReader) : IO Unit := do
  unless ← reader.active.get do
    reader.active.set true
    let _task ← IO.asTask do
      try
        reader.result.set (some (← readEvent))
      finally
        reader.active.set false
        reader.signal.notify

/-- Configuration for cooperative background jobs.

Multiple submitted lines may run at the same time. The renderer remains the
owner of `Model` and `Screen`; each worker returns only model state which is
merged through `finish` when its result is drained.
-/
structure JobConfig (Model : Type) where
  shouldRun : Model → String → Bool := fun _ _ => true
  start : Model → String → Model
  run : Cancellation → Model → String → IO Model
  /-- Poll shared background state and advance live job presentation. -/
  tick : Model → IO Model := pure
  finish : Model → Model → Model := fun _ completed => completed
  cancel : Model → Model := id
  fail : Model → String → Model := fun model _ => model

/-! ## Application keymaps -/

structure AppKeymap (Model : Type) where
  Action : Type
  keymap : Keymap Action
  contexts : Model → List String := fun _ => []
  handle : Model → Action → Option Model

def defaultFallbackSize : Size := { columns := 80, rows := 24 }

def defaultTickMs : UInt32 := 60

structure Config (Model : Type) where
  initial : Model
  inputConfig : TextInputConfig
  multiline : Option MultilineConfig := none
  fallbackSize : Size := defaultFallbackSize
  tickMs : UInt32 := defaultTickMs
  /-- Deprecated compatibility field; resize checks now follow `tickMs`. -/
  resizeMs : UInt32 := defaultTickMs
  editorKeymap : Option (Keymap EditorAction) := none
  mouse : Bool := false
  view : Model → Size → Text
  complete : Model → TextInputState → IO (List Completion)
  /-- Declarative application bindings. The first matching binding wins. -/
  keymap : Option (AppKeymap Model) := none
  /-- Handle an application-specific key before the REPL edits its input. -/
  handleKey : Model → Key → Option Model := fun _ _ => none
  /-- Handle an application mouse event before the REPL ignores it. -/
  handleMouse : Model → Size → MouseEvent → Option Model := fun _ _ _ => none
  getState : Model → State
  setState : Model → State → Model
  submit : Model → String → IO Model
  jobs : Option (JobConfig Model) := none
  isRunning : Model → Bool
  quit : Model → Model

def currentSize (fallback : Size) : IO Size := do
  pure ((← terminalSize).getD fallback)

private def waitForEvent (tickMs : UInt32) (signal : Std.Notify)
    (wakeSignal : Option Std.Notify) : IO Bool := do
  match wakeSignal with
  | none =>
      IO.sleep tickMs
      pure true
  | some wakeSignal =>
      let timer ← (Std.Async.Selector.sleep
        (Std.Time.Millisecond.Offset.ofNat tickMs.toNat)).block
      (Std.Async.Selectable.one #[
        .case signal.selector (fun _ => pure false),
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
      if ← waitForEvent tickMs reader.signal wakeSignal then
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

private def readEventWithResizeAtSize (tickMs : UInt32) (fallback : Size) (screen : Screen)
    (render : Screen → Size → IO Screen) (wake : IO Bool := pure false)
    (reader : Option EventReader := none) (wakeSignal : Option Std.Notify := none) :
    IO (Screen × Option Event × Bool) := do
  let reader ← match reader with
    | some reader => pure reader
    | none => newEventReader
  ensureEventReading reader
  let mut screen := screen
  let mut size ← currentSize fallback
  let mut woken := false
  while !woken && (← reader.result.get).isNone do
    if ← wake then
      woken := true
    else
      if ← waitForEvent tickMs reader.signal wakeSignal then
        let nextSize ← currentSize fallback
        if nextSize != size then
          screen ← render screen nextSize
          size := nextSize
  if woken then
    pure (screen, none, true)
  else
    let event := (← reader.result.get).getD none
    reader.result.set none
    reader.active.set false
    pure (screen, event, false)

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
    let loop : IO Unit := withRawInput do
      let mut model := config.initial
      let mut screen ← Screen.start
      let mut activeJobs : List (JobRuntime Model) := []
      let reader ← newEventReader
      let wakeSignal ← Std.Notify.new
      let mut dirty := true
      let frameNanos := config.tickMs.toNat * 1_000_000
      let mut nextRender : Nat := 0
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
        if !activeJobs.isEmpty then
          match config.jobs with
          | some jobs =>
              model ← jobs.tick model
              dirty := true
          | none => pure ()
        let now ← IO.monoNanosNow
        if dirty && now >= nextRender then
          screen ← render config screen model
          dirty := false
          nextRender := now + frameNanos
        let wake : IO Bool := do
          for runtime in activeJobs do
            if (← runtime.result.get).isSome then
              return true
          if dirty then
            let now ← IO.monoNanosNow
            if now >= nextRender then
              return true
          pure false
        let (nextScreen, event, woken) ←
          readEventWithResizeAtSize config.tickMs config.fallbackSize screen
          (fun screen size => renderAtSize config screen model size) wake (some reader)
          (some wakeSignal)
        screen := nextScreen
        match event with
        | none =>
            if !woken && activeJobs.isEmpty then
              model := config.quit model
        | some (.mouse mouse) =>
            let size ← currentSize config.fallbackSize
            match config.handleMouse model size mouse with
            | some nextModel =>
                model := nextModel
                dirty := true
            | none => pure ()
        | some (.key key) =>
            let appKey : Option (Option Model) := match config.keymap with
              | none => none
              | some keymap =>
                  match keymap.keymap.resolveBinding (keymap.contexts model) key with
                  | none => none
                  | some binding => some (keymap.handle model binding.action)
            match appKey with
            | some nextModel =>
                model := nextModel
                dirty := true
            | none =>
                match config.handleKey model key with
                | some nextModel =>
                    model := nextModel
                    dirty := true
                | none =>
                    let currentState := config.getState model
                      let editorAction := match config.multiline with
                      | some multiline =>
                          let keymap := multiline.keymap.getD
                            (config.editorKeymap.getD (defaultEditorKeymap multiline.lineBreak))
                          let context :=
                            { completionOpen := currentState.completion.isSome, multiline := true }
                          keymap.resolve (editorContexts context) key
                      | none =>
                          (config.editorKeymap.getD defaultEditorKeymap).resolve (editorContexts
                            { completionOpen := currentState.completion.isSome }) key
                    if editorAction == some .quit || editorAction == some .forceQuit then
                      match config.jobs, activeJobs.isEmpty with
                      | some jobs, false =>
                          for runtime in activeJobs do
                            runtime.cancellation.cancel
                          model := jobs.cancel model
                          activeJobs := []
                          dirty := true
                      | _, _ =>
                          if editorAction == some .forceQuit then
                            model := config.quit model
                          else
                            let (state, action) := match config.multiline with
                              | some multiline =>
                                  TermColor.Repl.updateMultilineWithKeymap multiline
                                  (multiline.keymap.getD
                                    (config.editorKeymap.getD
                                      (defaultEditorKeymap multiline.lineBreak)))
                                  (fun _ => []) currentState key
                              | none => TermColor.Repl.updateWithKeymap config.inputConfig
                                  (config.editorKeymap.getD defaultEditorKeymap)
                                  (fun _ => []) currentState key
                            model := config.setState model state
                            dirty := true
                            if action == .quit then
                              model := config.quit model
                    else
                      let candidates ←
                        if editorAction == some .complete && currentState.completion.isNone then
                          config.complete model currentState.input
                        else pure []
                      let (state, action) := match config.multiline with
                        | some multiline => TermColor.Repl.updateMultilineWithKeymap multiline
                            (multiline.keymap.getD
                              (config.editorKeymap.getD (defaultEditorKeymap multiline.lineBreak)))
                            (fun _ => candidates) currentState key
                        | none => TermColor.Repl.updateWithKeymap config.inputConfig
                            (config.editorKeymap.getD defaultEditorKeymap)
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
                                screen := Screen.empty
                                dirty := true
                          | none =>
                              model ← config.submit model line
                              screen := Screen.empty
                              dirty := true
    if config.mouse then
      withMouseCapture loop
    else
      loop
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
