# AGENTS.md

Emacs configuration — mostly built-in code with a few GNU ELPA packages
(`consult`, `vertico`, `orderless`) and vendored `company-mode` in `lisp/`.
See `init.el` for the load order.

## Structure

- `init.el` — entrypoint; installs GNU ELPA packages, then loads modules via `(my/load "...")` from `lisp/`
- `lisp/` — all configuration, grouped by concern:
  - `editor.el`, `ui.el`, `completion.el`, `windows.el`, `leader.el`, `treesitter.el`
  - `langs/` — per-language config (go, ts, csharp, markdown, docker)
  - `tools/` — git, eshell
  - `ace-window.el`, `hl-todo.el`, `vc-gutter.el` — custom implementations replacing popular packages
  - `company.el`, `company-capf.el`, `company-dabbrev.el`, `company-dabbrev-code.el` — vendored company-mode

## Key Conventions

- **Requires**: built-ins first (`cl-lib`, `subr-x`, `project`, `treesit`, `eglot`, `url`, `vc`), then project modules. No MELPA packages; only GNU ELPA (`consult`, `vertico`, `orderless`).
- `;; -*- lexical-binding: t; -*-` in every `.el` file.
- Standard file header, `(provide '...)`, and `;;; <file>.el ends here` trailer.
- 2-space indentation (Emacs Lisp default). Lines wrap around 80–100 cols.
- Private helpers prefix with `my/` (e.g., `my/load`, `my/ace-window`). Minor mode and public-facing symbols use descriptive names like `vc-gutter-mode`.
- Docstrings for all public-facing defuns, defcustoms, defvars. First sentence ends with period. Args in ALL CAPS.

## External Packages (GNU ELPA only)

| Package     | Purpose |
|---|---|
| `vertico`   | Minibuffer vertical completion UI |
| `orderless` | Flexible space-separated matching |
| `consult`   | Enhanced commands (buffer, ripgrep, line, imenu, goto-line, bookmark) |

## Custom Implementations (replacing popular packages)

| Replaced package  | Replacement file / approach |
|---|---|
| `magit`           | `lisp/tools/git.el` — built-in `vc` (vc-dir, vc-print-log, vc-annotate, vc-git-stash) |
| `avy`             | `lisp/ace-window.el` — custom window chooser |
| `dirvish` / `ranger` | Dired with `dired-hide-details-mode`, `dired-dwim-target` |
| `yasnippet`       | Built-in abbrev + skeleton |
| `multiple-cursors`| Built-in kmacro |
| `which-key`       | Built-in `which-key-mode` (Emacs 30+) |
| `diff-hl`         | `lisp/vc-gutter.el` — fringe overlay |
| `projectile`      | Built-in `project.el` with `vc` backend + extra root markers (`go.mod`, `.project`) |
| `hl-todo`         | `lisp/hl-todo.el` — font-lock keywords |

## Testing / Verification

No test framework. To check a `.el` file for errors:

```sh
emacs -Q -L lisp -batch -f batch-byte-compile lisp/<module>.el
```

For the full config: `emacs --batch --eval '(load "~/.emacs.d/init.el")'`.

## Tree-Sitter Grammars

Defined in `lisp/treesitter.el` (`my/treesit-langs`). After a fresh Emacs install,
run `M-x my/treesit-install-grammars` to download and compile all grammars.

## Leader Keybindings

Leader is `M-SPC` (defined in `lisp/leader.el`):
- `.` find-file, `,` consult-buffer, `f f` find-file, `f r` recentf
- `b b` / `b s` / `b .` consult-buffer, `b r` consult-buffer-other-window, `b p` previous-buffer, `b n` next-buffer, `b k` kill-current-buffer
- `/` consult-ripgrep, `s r` consult-ripgrep, `s l` consult-line, `s i` consult-imenu
- `w /` split-right, `w -` split-below, `w d` delete-window, `w o` delete-other-windows; `w <dir>` buf-move
- `g g` vc-dir, `g l` vc-log-toggle, `g b` vc-blame, `g s` / `g S` stash/pop, `g ,` / `g .` goto-last-change
- `p p` project-switch-project, `p f` project-find-file
- `<tab>` workspace layout (dired + code + eshell)
- `d` dired, `'` / `e` eshell-toggle, `E` eval-buffer
- `t d` dired-sidebar toggle, `t l` / `t n` line-numbers toggle
- `j` ace-window, `l` consult-goto-line
- `m m/e/x` kmacro start/end/call
- `n` consult-bookmark
- `h f/v/k/d` describe-function/variable/key/local-help
