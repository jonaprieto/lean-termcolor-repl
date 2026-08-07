# termcolor-repl

[![CI](https://github.com/jonaprieto/lean-termcolor-repl/workflows/CI/badge.svg)](https://github.com/jonaprieto/lean-termcolor-repl/actions/workflows/ci.yml)
[![Lean 4](https://img.shields.io/badge/Lean%204-library-5f5f5f)](lean-toolchain)
[![License](https://img.shields.io/badge/license-Apache--2.0-green)](LICENSE)

Pure input history, key handling, and adaptive completion for Lean 4 terminal REPLs.

`termcolor-repl` is the reusable layer above
[`termcolor-terminal`](https://github.com/jonaprieto/lean-termcolor-terminal). It deliberately
keeps application models, transcript rendering, command effects, and diagnostics in the client.

## Install

```toml
[[require]]
name = "termcolor-repl"
git = "https://github.com/jonaprieto/lean-termcolor-repl"
rev = "main"
```

## Current API

`TermColor.Repl.State` stores the editable input and submitted history. `update` consumes a
`termcolor-widgets` `Key` and returns an `Action`; applications provide completion candidates for
the current `TextInputState`.

`TermColor.Repl.Terminal` also provides terminal-size lookup and key waiting with live resize
redraws, plus a callback-driven `run` loop and `suspend` boundary for transient commands.

`TermColor.Repl.FileCompletion` provides bounded, hidden-file-aware completion for the token under
the cursor. Terminal callbacks return `IO (List Completion)` so filesystem candidates are read only
when the user presses tab.

Multiple candidates stay in a bounded `CompletionMenu`; tab and arrow keys select candidates, enter
accepts the selection, and escape dismisses it.

Pass `MultilineConfig` to `TermColor.Repl.Terminal.Config.multiline` to opt into multiline input;
the configured line-break key inserts a newline while enter still submits one logical history item.

Persistent history is opt-in through `TermColor.Repl.History`; `loadHistory` and `saveHistory`
return `Except` values so a missing or unwritable history file does not alter the active session.

```lean
import TermColor.Repl

open TermColor
open TermColor.Repl
open TermColor.Widgets

def complete : TextInputState → List Completion
  | input =>
      ["/help", "/history"].filter (·.startsWith input.value) |>.map
        (fun replacement => { replacement })

def handle (state : State) (key : Key) : State × Action :=
  update { width := 120, maxLength := 120 } complete state key
```

One candidate replaces the input. Multiple candidates expand only to their shared prefix, so
completion adapts naturally from commands to command options without knowing either domain.

## Development

```sh
lake build TermColor.Repl
python3 scripts/style-check.py
```

See [TODO.md](TODO.md) for intentionally deferred features.

## License

Apache-2.0.
