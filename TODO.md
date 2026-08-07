# TODO

## Reusable REPL features

- [x] Extract the full terminal runner around `Screen`, raw input, and submission callbacks.
  [Issue #6](https://github.com/jonaprieto/lean-termcolor-repl/issues/6)
- [x] Add a redraw/suspend boundary for commands that own transient live widgets.
  [Issue #5](https://github.com/jonaprieto/lean-termcolor-repl/issues/5)
- [ ] Support multiline input when a real client needs it.
  [Issue #1](https://github.com/jonaprieto/lean-termcolor-repl/issues/1)
- [x] Add optional persistent history with explicit empty-line and duplicate handling.
  [Issue #3](https://github.com/jonaprieto/lean-termcolor-repl/issues/3)
- [ ] Add file completion only when a client needs filesystem candidates.
  [Issue #2](https://github.com/jonaprieto/lean-termcolor-repl/issues/2)
- [ ] Add completion menus or fuzzy matching only after a second client establishes
  the interaction requirements.
-  [Issue #7](https://github.com/jonaprieto/lean-termcolor-repl/issues/7)
- [ ] Add selectable Vi/Emacs editing modes only when the underlying widget layer
  exposes the required key semantics.
-  [Issue #4](https://github.com/jonaprieto/lean-termcolor-repl/issues/4)
- [ ] Add background jobs and cancellation only when a client has long-running
  concurrent commands.
  [Issue #8](https://github.com/jonaprieto/lean-termcolor-repl/issues/8)

## Release

- [x] Add generated API documentation workflow.
- [x] Publish the `0.1.0` tagged release.
