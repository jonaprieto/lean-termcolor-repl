/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import Argus
import TermColor.Repl.FileCompletion

/-!
# TermColor.Repl.Command

An interactive adapter for Argus' inspectable command specifications. The command grammar,
usage metadata, and finite/path completions all come from the same `Argus.Command`.
-/

namespace TermColor.Repl

open Argus
open TermColor.Widgets

abbrev CommandSpec (Action : Type) := Argus.Command Action

structure CommandWord where
  value : String
  start : Nat
  stop : Nat
deriving Repr, BEq, DecidableEq

private def whitespace (character : Char) : Bool :=
  character == ' ' || character == '\t' || character == '\n'

private def finishWord (current : List Char) (start stop : Nat)
    (words : List CommandWord) : List CommandWord :=
  if current.isEmpty then words
  else { value := String.ofList current.reverse, start, stop } :: words

private def scanWords : List Char → Nat → List Char → Nat → List CommandWord → List CommandWord
  | [], index, current, start, words => finishWord current start index words
  | character :: rest, index, current, start, words =>
      if whitespace character then
        let words := finishWord current start index words
        scanWords rest (index + 1) [] (index + 1) words
      else
        scanWords rest (index + 1) (character :: current)
          (if current.isEmpty then index else start) words

private def words (source : String) : List CommandWord :=
  (scanWords source.toList 0 [] 0 []).reverse

private def cursorRange (input : TextInputState) : Nat × Nat :=
  let chars := input.value.toList
  let cursor := min input.cursor chars.length
  let before := chars.take cursor
  let start := cursor - (before.reverse.takeWhile (fun character => !whitespace character)).length
  let after := chars.drop cursor
  let stop := cursor + (after.takeWhile (fun character => !whitespace character)).length
  (start, stop)

private def cursorWord (input : TextInputState) : String :=
  let (start, stop) := cursorRange input
  String.ofList (input.value.toList.drop start |>.take (stop - start))

private def inputBeforeCursor (input : TextInputState) : String :=
  String.ofList (input.value.toList.take (min input.cursor input.value.toList.length))

private def children {α : Type} : Argus.Command α → List (Argus.Command α)
  | { body := .subs children, .. } => children
  | _ => []

private def childByName {α : Type} (root : Argus.Command α) (name : String) :
    Option (Argus.Command α) :=
  (children root).find? (·.name == name)

private def commandNames {α : Type} (root : Argus.Command α) : List String :=
  (children root).map (fun command => "/" ++ command.name)

private def candidate (input : TextInputState) (replacement : String) : Completion :=
  let (start, stop) := cursorRange input
  { replacement, range := some (start, stop) }

private def commandCandidates {α : Type} (root : Argus.Command α) (input : TextInputState) :
    List Completion :=
  let fragment := cursorWord input
  (commandNames root).filter (·.startsWith fragment) |>.map (candidate input)

private def flagWords (flags : List Argus.FlagInfo) : List String :=
  flags.flatMap fun flag =>
    let long := ["--" ++ flag.long]
    match flag.short with
    | some short => long ++ ["-" ++ short.toString]
    | none => long

private def flagTakesValue (flag : String) (info : Argus.FlagInfo) : Bool :=
  flag == "--" ++ info.long ||
    match info.short with
    | some short => flag == "-" ++ short.toString
    | none => false

private def previousWord (input : TextInputState) : Option String :=
  let cursor := min input.cursor input.value.toList.length
  (words (String.ofList (input.value.toList.take cursor))).reverse.drop 1 |>.head?.map (·.value)

private def listGet? {α : Type} : List α → Nat → Option α
  | [], _ => none
  | value :: _, 0 => some value
  | _ :: rest, index + 1 => listGet? rest index

private def valueInfo {α : Type} (command : Argus.Command α) (input : TextInputState) :
    Option String :=
  let metadata := command.toMeta
  match previousWord input with
  | some previous => metadata.flags.find? (flagTakesValue previous) |>.bind (·.typeName)
  | none => none

private def positionalInfo {α : Type} (command : Argus.Command α) (input : TextInputState) :
    Option Argus.ArgInfo :=
  let metadata := command.toMeta
  let tokens := words (inputBeforeCursor input) |>.drop 1
  let count := tokens.countP (fun token => !token.value.startsWith "-")
  listGet? metadata.args (min count (max 0 (metadata.args.length - 1)))

private def valueCandidates (input : TextInputState) (values : List String) : List Completion :=
  let fragment := cursorWord input
  let hasSlash := fragment.startsWith "/"
  let normalizedValue := if hasSlash then fragment.drop 1 |>.toString else fragment
  let replacementPrefix := if hasSlash then "/" else ""
  values.filter (·.startsWith normalizedValue) |>.map
    (fun value => candidate input (replacementPrefix ++ value))

private def argumentCandidates {α : Type} (command : Argus.Command α) (input : TextInputState)
    (values : String → IO (List String)) :
    IO (List Completion) := do
  match valueInfo command input with
  | some typeName =>
      if typeName == "PATH" then defaultFileCompletions input
      else
        let values ← values typeName
        pure (valueCandidates input values)
  | none =>
      match positionalInfo command input with
      | some info =>
          if info.typeName == "PATH" then defaultFileCompletions input
          else
            let values ← values info.typeName
            pure (valueCandidates input values)
      | none => pure []

private def optionCandidates {α : Type} (command : Argus.Command α) (input : TextInputState) :
    List Completion :=
  let fragment := cursorWord input
  (flagWords command.toMeta.flags).filter (·.startsWith fragment) |>.map (candidate input)

private def exactCommand {α : Type} (root : Argus.Command α) (input : TextInputState) :
    Option (Argus.Command α) :=
  match words (inputBeforeCursor input) with
  | first :: _ => childByName root (first.value.drop 1).toString
  | [] => none

/-- Parse a slash command using the supplied Argus command group. -/
def parseCommand {Action : Type} (root : CommandSpec Action) (source : String) :
    Except String Action :=
  let line := source.trimAscii.toString
  if !line.startsWith "/" then
    .error "input is not a slash command"
  else
    let argv := (words line).map (fun word => word.value)
    match argv with
    | [] => .error "empty command"
    | command :: args =>
        let command := command.drop 1 |>.toString
        match root.run (command :: args) with
        | .ok action => .ok action
        | .error errors => .error (String.intercalate "\n" (errors.map Err.message))

/-! Complete finite values through a type-name resolver; the command grammar still supplies the
field location, option order, and path behavior. This keeps dynamic catalogues out of the parser. -/
def completeCommandWith {Action : Type} (root : CommandSpec Action)
    (values : String → IO (List String)) (input : TextInputState) :
    IO (List Completion) := do
  let before := inputBeforeCursor input
  let value := before.trimAscii.toString
  if !value.startsWith "/" then
    return []
  let tokens := words value
  if tokens.length ≤ 1 && !before.endsWith " " then
    return commandCandidates root input
  match exactCommand root input with
  | none => pure (commandCandidates root input)
  | some command =>
      let fragment := cursorWord input
      if fragment.startsWith "-" || (valueInfo command input).isNone then
        let options := optionCandidates command input
        if options.isEmpty then argumentCandidates command input values else pure options
      else
        argumentCandidates command input values

/- Complete a slash command, using command metadata for names, options, and paths. -/
def completeCommand {Action : Type} (root : CommandSpec Action) (input : TextInputState) :
    IO (List Completion) :=
  completeCommandWith root (fun _ => pure []) input

structure CommandHelp where
  name : String
  usage : String
  description : String
deriving Repr, BEq, DecidableEq

/-- Help rows derived from the same command group used by `parseCommand`. -/
def commandHelp {Action : Type} (root : CommandSpec Action) : List CommandHelp :=
  (children root).map fun command =>
    { name := "/" ++ command.name
      usage := "/" ++ command.usageLine
      description := command.description }

end TermColor.Repl
