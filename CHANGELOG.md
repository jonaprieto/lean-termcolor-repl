# Changelog

## 0.8.7 — 2026-09-08

- Support Lean v4.33.1 and refresh Terminal and Argus dependencies.
- Clarify AI-assisted development.

## 0.8.6 — 2026-08-13

- Add the standard review guidance to the README.
- Pin the newest released TermColor Terminal and Argus dependencies.

## 0.8.5 — 2026-08-13

- Publish the final Lean 4.33 documentation artifact fix.

## 0.8.4 — 2026-08-13

- Publish the dependency-graph README cleanup.

## 0.8.3 — 2026-08-13

- Adopt precommit-lean v0.1.6.

## 0.8.2 — 2026-08-12

- Adopt Lean v4.33.0 and precommit-lean v0.1.5.

## 0.8.1

- make event readers cancellation-aware and join them during terminal cleanup;
- prevent a pending event from being overwritten by a second reader;
- retain and cancel/join background job tasks during shutdown.

## 0.8.0

- consume matched application bindings even when their handler leaves the model unchanged;
- complete nested commands and consume Argus structural cursor metadata;
- replace stringly typed editor/application contexts with `KeyContext` values;
- expose binding conflict inspection and add executable completion/property checks;
- retain `Config.resizeMs` as a compatibility field while resize handling follows `tickMs`.
