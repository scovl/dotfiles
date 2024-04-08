;; Ensure that use-package is available
(require 'use-package)

;; lsp-mode configuration
(use-package lsp-mode
  :ensure t
  :hook (c-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-prefer-flymake nil) ; Prefer lsp-ui (flycheck) over flymake unless specified
  (setq lsp-clients-clangd-args '("--header-insertion=never")))

;; Optional: lsp-ui configuration for an enhanced UI experience with lsp-mode
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions t))

;; Optional: Company configuration for autocompletion
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)) ;; Show suggestions immediately

;; Optional: lsp-mode integration with treemacs for project tree visualization
(use-package lsp-treemacs
  :ensure t
  :after lsp)

;; Additional configuration to enhance the experience with C/C++
(add-hook 'c-mode-hook (lambda ()
                         (setq c-basic-offset 4) ;; Set indentation to 4 spaces
                         (setq-default tab-width 4) ;; Set tab width to 4 spaces
                         (setq-default c-default-style "linux") ;; Coding style
                         (electric-pair-mode 1))) ;; Auto-close parentheses and quotes

;; Ensure lsp-mode starts automatically for C
(add-hook 'c-mode-hook #'lsp-deferred)

(defun my-compile ()
  "Custom function to compile C projects.
   Change the compilation command as needed."
  (interactive)
  ;; Example using make
  ;; Ensure your project has a Makefile
  (compile "make -k")
  ;; If you want to compile a single file with gcc or clang directly, you can replace the above line with
  ;; (compile "gcc -o program file.c")
  ;; or for clang
  ;; (compile "clang -o program file.c")
)

;; Configuring keybindings
(defun setup-c-mode-compile-keybindings ()
  "Configure keybindings for compilation in C mode."
  (local-set-key (kbd "C-c c") 'my-compile) ;; Compiles the project or file
)

;; Apply the keybindings when entering c-mode
(add-hook 'c-mode-hook 'setup-c-mode-compile-keybindings)
