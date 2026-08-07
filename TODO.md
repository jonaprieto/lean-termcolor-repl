# TODO

## Reusable REPL features

- [x] Extract the full terminal runner around `Screen`, raw input, and submission callbacks.
  [Issue #6](https://github.com/jonaprieto/lean-termcolor-repl/issues/6)
- [x] Add a redraw/suspend boundary for commands that own transient live widgets.
  [Issue #5](https://github.com/jonaprieto/lean-termcolor-repl/issues/5)
- [x] Support opt-in multiline input with a configurable line-break key.
  [Issue #1](https://github.com/jonaprieto/lean-termcolor-repl/issues/1)
- [x] Add optional persistent history with explicit empty-line and duplicate handling.
  [Issue #3](https://github.com/jonaprieto/lean-termcolor-repl/issues/3)
- [x] Add bounded, hidden-file-aware completion for filesystem candidates.
  [Issue #2](https://github.com/jonaprieto/lean-termcolor-repl/issues/2)
- [ ] Add completion menus or fuzzy matching only after a second client establishes
  the interaction requirements.
-  [Issue #7](https://github.com/jonaprieto/lean-termcolor-repl/issues/7)
- [ ] Revisit selectable Vi/Emacs editing modes when the underlying widget layer
  exposes the required key semantics. Closed for now.
  [Issue #4](https://github.com/jonaprieto/lean-termcolor-repl/issues/4)
- [ ] Add background jobs and cancellation only when a client has long-running
  concurrent commands.
  [Issue #8](https://github.com/jonaprieto/lean-termcolor-repl/issues/8)

## Versioned roadmap

- `0.2.0`: multiline input ([Issue #1](https://github.com/jonaprieto/lean-termcolor-repl/issues/1))
- `0.3.0`: file completion ([Issue #2](https://github.com/jonaprieto/lean-termcolor-repl/issues/2))
- `0.4.0`: completion presentation ([Issue #7](https://github.com/jonaprieto/lean-termcolor-repl/issues/7))
- `0.5.0`: background jobs ([Issue #8](https://github.com/jonaprieto/lean-termcolor-repl/issues/8))

## Release

- [x] Add generated API documentation workflow.
- [x] Publish the `0.1.0` tagged release.
- [x] Publish the `0.2.0` tagged release with multiline input.
