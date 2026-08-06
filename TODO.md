# TODO

## Reusable REPL features

- [ ] Extract the full terminal runner around `Screen`, raw input, and submission callbacks.
- [ ] Add a redraw/suspend boundary for commands that own transient live widgets.
- [ ] Support multiline input when a real client needs it.
- [ ] Add persistent history only when a client needs history across sessions.
- [ ] Add file completion only when a client needs filesystem candidates.
- [ ] Add completion menus or fuzzy matching only after a second client establishes
  the interaction requirements.
- [ ] Add selectable Vi/Emacs editing modes only when the underlying widget layer
  exposes the required key semantics.
- [ ] Add background jobs and cancellation only when a client has long-running
  concurrent commands.

## Release

- [x] Add generated API documentation workflow.
- [x] Publish the `0.1.0` tagged release.
