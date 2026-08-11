# Changelog

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
