# lean-termcolor-repl

[![CI](https://github.com/jonaprieto/lean-termcolor-repl/actions/workflows/ci.yml/badge.svg)](https://github.com/jonaprieto/lean-termcolor-repl/actions/workflows/ci.yml)
[![Release](https://img.shields.io/github/v/release/jonaprieto/lean-termcolor-repl?display_name=tag&sort=semver)](https://github.com/jonaprieto/lean-termcolor-repl/releases)
[![Lean 4](https://img.shields.io/badge/Lean%204-v4.33.1-6f42c1)](lean-toolchain)
[![Docs](https://img.shields.io/badge/docs-GitHub%20Pages-4c8bf5)](https://jonaprieto.github.io/lean-termcolor-repl/)
[![License](https://img.shields.io/badge/license-Apache--2.0-green)](LICENSE)

Pure input history, key handling, adaptive completion, and cooperative background-job support for
Lean 4 terminal REPLs.

## Status and review

These libraries are actively evolving and are developed with AI assistance and human review.
CI and machine-checked proofs provide useful evidence, but do not guarantee correctness,
soundness, portability, performance, or suitability for every use case. Validate behavior
and assumptions before relying on a release.

Reviewer feedback is welcome, especially on correctness, proofs, API design, usability,
portability, performance, documentation, and real-world use. Please use the
[issue tracker](https://github.com/jonaprieto/lean-termcolor-repl/issues) or open a PR with a
reproducible example and the expected behavior.

## Install

```lean
require termcolor-repl from git
  "https://github.com/jonaprieto/lean-termcolor-repl.git" @ "v0.8.2"
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
[`lean-calc-chat`](https://github.com/jonaprieto/lean-calc-chat) is a complete consumer;
[`argus`](https://github.com/jonaprieto/lean-argus) provides its command-line integration; and
[`oatp`](https://github.com/jonaprieto/oatp) uses the REPL layer.

## License

Apache-2.0.
