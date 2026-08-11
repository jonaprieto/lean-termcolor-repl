# termcolor-repl

[![CI](https://github.com/jonaprieto/lean-termcolor-repl/actions/workflows/ci.yml/badge.svg)](https://github.com/jonaprieto/lean-termcolor-repl/actions/workflows/ci.yml)
[![Lean 4](https://img.shields.io/badge/Lean%204-library-5f5f5f)](lean-toolchain)
[![License](https://img.shields.io/badge/license-Apache--2.0-green)](LICENSE)

Pure input history, key handling, adaptive completion, and cooperative background-job support for
Lean 4 terminal REPLs.

## Install

```lean
require termcolor-repl from git
  "https://github.com/jonaprieto/lean-termcolor-repl.git" @ "v0.7.2"
```

## API

`TermColor.Repl.State` stores editable input and submitted history. `update` maps widget keys to
pure actions. `Keymap.BindingSpec` and `Keymap.fromSpecs` keep editor/application bindings
inspectable, and `Config.editorKeymap` applies a custom editor map to single- and multiline input.
`FileCompletion` provides bounded completion for the token under the cursor, and `History` provides
opt-in file persistence through `Except` results.

`Config.resizeMs` remains as a compatibility field; resize checks are event-driven and use `tickMs`.

`TermColor.Repl.Terminal` adds terminal size lookup, resize-aware key input, multiline input,
transient-command suspension, and cooperative jobs. Each submitted line may run independently;
the renderer merges completed results while input and other jobs continue. `Config.handleKey` and
`Config.handleMouse` let an application consume its own input before normal REPL handling; set
`Config.mouse` to enable SGR mouse capture.

```lean
import TermColor.Repl

open TermColor TermColor.Repl TermColor.Widgets

def complete : TextInputState → List Completion
  | input => ["/help", "/history"].filter (·.startsWith input.value) |>.map
      (fun replacement => { replacement })

def handle (state : State) (key : Key) : State × Action :=
  update { width := 120, maxLength := 120 } complete state key
```

## Build

```sh
lake build TermColor.Repl TermColor.Repl.Properties tests
```

## Related projects

[`termcolor-terminal`](https://github.com/jonaprieto/lean-termcolor-terminal) owns terminal IO;
[`lean-calc-chat`](https://github.com/jonaprieto/lean-calc-chat) is a complete consumer.

## License

Apache-2.0.
