/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides.
-/

import TermColor.Repl

/-!
# TermColor.Repl.History

Opt-in persistence for submitted REPL lines. The in-memory `State.history` remains the default;
this module only supplies a small file backend for clients that want history across sessions.
-/

namespace TermColor.Repl

structure HistoryConfig where
  path : System.FilePath
  maxEntries : Nat := 500
  deduplicate : Bool := true
deriving Repr

private def trimHistory (maxEntries : Nat) (history : List String) : List String :=
  if history.length > maxEntries then history.drop (history.length - maxEntries) else history

/-- Normalize lines for storage, skipping empty lines and optionally keeping the newest duplicate. -/
def normalizeHistory (config : HistoryConfig) (lines : List String) : Array String :=
  let append (history : List String) (line : String) : List String :=
    let line := line.trimAscii.toString
    if line.isEmpty then history
    else
      let history := if config.deduplicate then history.filter (· != line) else history
      trimHistory config.maxEntries (history ++ [line])
  (lines.foldl append []).toArray

private def serializeHistory (history : Array String) : String :=
  if history.isEmpty then "" else String.intercalate "\n" history.toList ++ "\n"

/-- Read persisted history. Missing, unreadable, or malformed files return an error. -/
def loadHistory (config : HistoryConfig) : IO (Except String (Array String)) := do
  try
    let contents ← IO.FS.readFile config.path
    pure (.ok (normalizeHistory config (contents.splitOn "\n")))
  catch error =>
    pure (.error error.toString)

/-- Write history without changing the caller's active in-memory state on failure. -/
def saveHistory (config : HistoryConfig) (history : Array String) : IO (Except String Unit) := do
  try
    let normalized := normalizeHistory config history.toList
    IO.FS.writeFile config.path (serializeHistory normalized)
    pure (.ok ())
  catch error =>
    pure (.error error.toString)

end TermColor.Repl
