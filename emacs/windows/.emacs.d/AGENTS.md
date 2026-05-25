# AGENTS.md

Emacs Solo configuration — 100% built-in or custom `lisp/` code. Zero MELPA/ELPA
packages. See `init.el` for the load order.

## Structure

- `init.el` — entrypoint; loads modules via `(my/load "...")` from `lisp/`
- `lisp/` — all configuration, grouped by concern:
  - `editor.el`, `ui.el`, `completion.el`, `windows.el`, `leader.el`, `treesitter.el`
  - `langs/` — per-language config (go, ts, csharp, markdown, docker)
  - `tools/` — git, ai (OpenCode), eshell
  - `ace-window.el`, `hl-todo.el`, `vc-gutter.el`, `request-shim.el` — custom implementations replacing popular packages
- `jdormit-emacs-opencode/` — vendored OpenCode Emacs client (separate repo, has its own `AGENTS.md`; do not modify)

## Key Conventions

- **Requires**: built-ins first (`cl-lib`, `subr-x`, `project`, `treesit`, `eglot`, `url`, `vc`), then project modules. No external `require` calls.
- `;; -*- lexical-binding: t; -*-` in every `.el` file.
- Standard file header, `(provide '...)`, and `;;; <file>.el ends here` trailer.
- 2-space indentation (Emacs Lisp default). Lines wrap around 80–100 cols.
- Private helpers prefix with `my/` (e.g., `my/load`, `my/ace-window`). Minor mode and public-facing symbols use descriptive names like `vc-gutter-mode`.
- Docstrings for all public-facing defuns, defcustoms, defvars. First sentence ends with period. Args in ALL CAPS.

## Substituting Popular Packages (Built-in Alternatives)

| Replaced package  | Replacement file / approach |
|---|---|
| `magit`           | `lisp/tools/git.el` — built-in `vc` (vc-dir, vc-print-log, vc-annotate, vc-git-stash) |
| `avy`             | `lisp/ace-window.el` — ~40 LOC custom window chooser |
| `dirvish` / `ranger` | Dired with `dired-hide-details-mode`, `dired-dwim-target` |
| `yasnippet`       | Built-in abbrev + skeleton (`abbrev-file-name`, `define-abbrev`) |
| `multiple-cursors`| Built-in kmacro (`kmacro-start-macro`/`end-macro`/`call-macro`) |
| `request.el`      | `lisp/request-shim.el` — shim over `url-retrieve` |
| `which-key`       | Built-in `which-key-mode` (Emacs 30+) |
| `vertico` / `selectrum` | `icomplete-vertical-mode` + flex completion style |
| `diff-hl`         | `lisp/vc-gutter.el` — ~75 LOC fringe overlay |
| `projectile`      | Built-in `project.el` with `vc` backend + extra root markers (`go.mod`, `.project`) |
| `hl-todo`         | `lisp/hl-todo.el` — ~20 LOC font-lock hack |

## Testing / Verification

No test framework. To check a `.el` file for errors:

```sh
emacs -Q -L lisp -batch -f batch-byte-compile lisp/<module>.el
```

For the full config: `emacs --batch --eval '(load "~/.emacs.d/init.el")'`.

## Tree-Sitter Grammars

Defined in `lisp/treesitter.el` (`my/treesit-langs`). After a fresh Emacs install,
run `M-x my/treesit-install-grammars` to download and compile all grammars.

## OpenCode Integration

`lisp/tools/ai.el` loads the vendored client from `jdormit-emacs-opencode/` and
sets `opencode-server-command` to the opencode CLI path. The workspace layout
(`M-SPC <tab>`) spawns dired, eshell, and opencode side-by-side.

## Leader Keybindings

Leader is `M-SPC` (defined in `lisp/leader.el`):
- `.` find-file, `,` switch-to-buffer, `f f` find-file, `f r` recentf
- `b b` switch-to-buffer, `b k` kill-current-buffer
- `/` ripgrep, `s r` ripgrep, `s l` isearch, `s i` imenu
- `w /` split-right, `w -` split-below, `w d` delete-window, `w o` delete-other-windows; `w <dir>` buf-move
- `g g` vc-dir, `g l` vc-log-toggle, `g b` vc-blame, `g s` / `g S` stash/pop
- `p p` project-switch-project, `p f` project-find-file
- `<tab>` workspace layout (dired + code + opencode + eshell)
- `'` / `e` eshell-toggle, `E` eval-buffer, `o` opencode
- `t d` dired-sidebar toggle, `t l` / `t n` line-numbers toggle
- `j` ace-window, `l` goto-line
- `m m/e/x` kmacro start/end/call
- `n` bookmark-jump
- `h f/v/k/d` describe-function/variable/key/local-help
