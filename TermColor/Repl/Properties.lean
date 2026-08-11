/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import TermColor.Repl
import TermColor.Repl.History

namespace TermColor.Repl.Properties

open TermColor.Widgets

private def config : TextInputConfig := { width := 120, maxLength := 120 }

private def commandCompletion : TextInputState → List Completion
  | input =>
      ["/help", "/history"].filter (·.startsWith input.value) |>.map
        (fun replacement => { replacement })

private def historyConfig : HistoryConfig := { path := "history", maxEntries := 2 }

private def multilineConfig : MultilineConfig := { text := config }

private def customEditorKeymap : Keymap EditorAction :=
  Keymap.fromSpecs [{ keys := [.ctrl 's'], action := .submit, context := some "editor" }]

example :
    (recallUp { history := #["first", "second"] }).input.value = "second" := by
  native_decide

example :
    (recallDown { history := #["first", "second"], historyIndex := some 0 }).input.value =
      "second" := by
  native_decide

example :
    (completeInput { value := "/he", cursor := 3 }
      [{ replacement := "/help" }]).value = "/help" := by
  native_decide

example :
    (completeInput { value := "/h", cursor := 2 }
      [{ replacement := "/help" }, { replacement := "/history" }]).value = "/h" := by
  native_decide

example :
    (completeInput { value := "/load ma.txt", cursor := 12 }
      [{ replacement := "main.lean", range := some (6, 12) }]).value = "/load main.lean" := by
  native_decide

example :
    (update config commandCompletion { input := { value := "/", cursor := 1 }} .tab).1.completion =
      some { candidates := #[{ replacement := "/help" }, { replacement := "/history" }] } := by
  native_decide

example :
    (renderCompletionMenu { width := 8 } { candidates := #[{ replacement := "long" }] }).plainText
      |>.splitOn "\n" |>.all (·.length ≤ 8) := by
  native_decide

example :
    (update config commandCompletion { input := { value := "2+2", cursor := 3 }} .enter).2 =
      .submit "2+2" := by
  native_decide

example :
    normalizeHistory historyConfig [" first ", "", "second", "first", "third"] =
      #["first", "third"] := by
  native_decide

example :
    normalizeHistory { historyConfig with deduplicate := false } ["first", "first"] =
      #["first", "first"] := by
  native_decide

example :
    (updateMultiline multilineConfig (fun _ => [])
      { input := { value := "1+2", cursor := 3 }} (.ctrl 'n')).1.input.value = "1+2\n" := by
  native_decide

example :
    (updateMultiline multilineConfig (fun _ => [])
      { input := { value := "a\nbc", cursor := 4 }} .up).1.input.cursor = 1 := by
  native_decide

example :
    (updateMultiline multilineConfig (fun _ => [])
      { input := { value := "a\nb", cursor := 3 }} .enter).2 = .submit "a\nb" := by
  native_decide

example :
    (defaultEditorKeymap.resolve ["editor"] .enter) = some .submit := by
  native_decide

example :
    (defaultEditorKeymap.resolve ["editor", "completion"] .escape) =
      some .dismissCompletion := by
  native_decide

example :
    (defaultEditorKeymap.resolve ["multiline", "editor"] (.ctrl 'n')) =
      some .lineBreak := by
  native_decide

example :
    (customEditorKeymap.resolve ["editor"] (.ctrl 's')) = some .submit := by
  native_decide

example :
    (updateWithKeymap config customEditorKeymap (fun _ => [])
      { input := { value := "p => p", cursor := 6 }} (.ctrl 's')).2 =
      .submit "p => p" := by
  native_decide

example :
    (updateMultilineWithKeymap multilineConfig customEditorKeymap (fun _ => [])
      { input := { value := "p => p", cursor := 6 }} (.ctrl 's')).2 =
      .submit "p => p" := by
  native_decide

example : Keymap.keyLabel (.ctrl 's') = "Ctrl-s" := by
  native_decide

end TermColor.Repl.Properties
