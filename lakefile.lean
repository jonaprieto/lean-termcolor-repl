import Lake
open Lake DSL

package «termcolor-repl» where
  version := v!"0.7.2"
  leanOptions := #[⟨`autoImplicit, false⟩, ⟨`relaxedAutoImplicit, false⟩]

require «termcolor-terminal» from git
  "https://github.com/jonaprieto/lean-termcolor-terminal.git"
  @ "v0.3.1"

require argus from git
  "https://github.com/jonaprieto/lean-argus.git"
  @ "v0.4.8"

@[default_target]
lean_lib «TermColor.Repl» where
  roots := #[`TermColor.Repl]
  globs := #[.andSubmodules `TermColor.Repl]

lean_lib «TermColor.Repl.Properties» where
  roots := #[`TermColor.Repl.Properties]
  globs := #[.andSubmodules `TermColor.Repl.Properties]

@[test_driver]
lean_exe tests where
  root := `Tests
  srcDir := "test"
