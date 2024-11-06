# Tree-sitter Language Bundle for Emacs

__This is fork of <https://github.com/emacs-tree-sitter/tree-sitter-langs>__ that includes more parsers.

This is a convenient language bundle for the Emacs package [tree-sitter](https://github.com/emacs-tree-sitter/elisp-tree-sitter). It serves as an interim distribution mechanism, until `tree-sitter` is widespread enough for language-specific major modes to incorporate its functionalities.

For each supported language, this package provides:

1.  Pre-compiled grammar binaries for 3 major platforms: macOS, Linux and Windows, on x86\_64. In the future, `tree-sitter-langs` may provide tooling for major modes to do this on their own.
2.  An optional `highlights.scm` file that provides highlighting patterns. This is mainly intended for major modes that are not aware of `tree-sitter`. A language major mode that wants to use `tree-sitter` for syntax highlighting should instead provide the query patterns on its own, using the mechanisms defined by [tree-sitter-hl](https://emacs-tree-sitter.github.io/syntax-highlighting/interface-for-modes/).
3.  Optional query patterns for other minor modes that provide high-level functionalities on top of `tree-sitter`, such as code folding, evil text objects… As with highlighting patterns, major modes that are directly aware of `tree-sitter` should provide the query patterns on their own.

## Highlighting Queries

Highlighting query patterns for a language are in the file `queries/<lang>/highlights.scm`. Most of them are __intentionally different__ from those from upstream repositories, which are more geared towards *GitHub's use cases*. We try to be more consistent with *Emacs's existing conventions*. (For some languages, this is WIP, so their patterns may look similar to upstream's.)

In general, try to follow what the docstrings of `treesit-face-` faces say. Most importantly:

- Definitions and uses should be differentiated:
  - `@function` vs. `@function.call`.
  - `@method` vs. `@method.call`.
  - `@type.parameter` vs. `@type.argument`.
- `@variable` and `@variable.parameter` should be applied only to declarations/definitions/bindings/mutations (*writes*), not usage (*reads*).
- Special faces should have high priority (placed earlier in the pattern list): `@function.macro`, `@type.builtin`, `@variable.special`.
- Patterns whose internals may be highlighted should have low priority (placed towards the end). Example: strings with interpolation.

### Mode-specific highlighting

Some languages are associated with multiple major modes. Mode-specific highlighting patterns are provided by the files `queries/<lang>/highlights.<major-mode>.scm`. These are combined with the base highlighting patterns in `queries/<lang>/highlights.scm`, but have higher precedence.

## Building Grammars from Source

### Tools and dependencies

- Install NodeJS. It is needed to generate the grammar code from the JavaScript DSL. The recommended tool to manage NodeJS is [volta](https://volta.sh/).
- Install [tree-sitter CLI tool](https://tree-sitter.github.io/tree-sitter/creating-parsers#installation). (Its binary can also be downloaded directly from [GitHub](https://github.com/tree-sitter/tree-sitter/releases).)

### Building grammars

To build a specific language's grammar, run `script/compile`. (See the list of registered languages in `treesit-langs-source-alist`.) For example:

``` bash
script/compile rust
```

To build all registered languages, and creating the bundle:

``` bash
script/compile all
```

### Adding a new grammar

- Add a new entry in [`grammars`](./script/_grammars) and optionally [`treesit-langs-source-alist`](./treesit-langs-build.el).
  The entry is in the format of `(lang :url <git-url> :rev <revision> :src <src-dir>)`.
- Add the `(major-mode . language)` to the `treesit-major-mode-language-alist`
- Add the highlighting rules to the `queries/<lang>/highlights.scm`
