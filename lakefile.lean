import Lake
open Lake DSL

package «termcolor-repl» where
  version := v!"0.6.0"
  leanOptions := #[⟨`autoImplicit, false⟩, ⟨`relaxedAutoImplicit, false⟩]

require «termcolor-terminal» from git
  "https://github.com/jonaprieto/lean-termcolor-terminal.git"
  @ "v0.2.0"

@[default_target]
lean_lib «TermColor.Repl» where
  roots := #[`TermColor.Repl]
  globs := #[.andSubmodules `TermColor.Repl]

lean_lib «TermColor.Repl.Properties» where
  roots := #[`TermColor.Repl.Properties]
  globs := #[.andSubmodules `TermColor.Repl.Properties]
