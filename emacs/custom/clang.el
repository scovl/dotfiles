;; lsp-mode configuration
(use-package lsp-mode
  :ensure t
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-keymap-prefix "C-c l")
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (setq lsp-file-watch-threshold 15000))

(use-package lsp-ui
  :ensure t
  :commands (lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-delay 0.5)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;; company
(use-package company
  :ensure t
  :bind ("M-/" . company-complete-common-or-cycle) ;; overwritten by flyspell
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-show-numbers            t
  company-minimum-prefix-length   1
  company-idle-delay              0.5
  company-backends
  '((company-files          ; files & directory
     company-keywords       ; keywords
     company-capf           ; what is this?
     company-yasnippet)
    (company-abbrev company-dabbrev))))

(use-package company-box
  :ensure t
  :after company
  :hook (company-mode . company-box-mode))

;; flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function
  #'flycheck-display-error-messages-unless-error-list)

  (setq flycheck-indication-mode nil))

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (flycheck-pos-tip-mode))

(use-package ccls
  :ensure t
  :config
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp)))
  (setq ccls-executable "/usr/local/bin/ccls")
  (setq ccls-args nil)
  (setq ccls-initialization-options
  '(:index (:comments 2) :completion (:detailedLabel t))))

(defun generate-ccls-config ()
  "Generate .ccls file in root project"
  (interactive)
  (let ((root (locate-dominating-file (buffer-file-name) "src"))  ; Detecta a raiz do projeto baseado na existência de um diretório 'src'
        (ccls-content '("%clang"
                        "%c -std=gnu11"
                        "%cpp -std=gnu++14")))
    (when root
      (with-temp-buffer
        ;; Add the 'include' directory if it exists
        (when (file-exists-p (concat root "include"))
          (setq ccls-content (append ccls-content '("-Iinclude"))))

        ;; Write the settings in the buffer and save them to the .ccls file
        (dolist (line ccls-content)
          (insert line "\n"))
        (write-file (concat root ".ccls"))
        (message "Arquivo .ccls gerado em: %s" root)))))

;; Configuring keybindings
(defun setup-c-mode-compile-keybindings ()
  "Configure keybindings for compilation in C mode."
  (local-set-key (kbd "C-c c") 'my-compile) ;; Compiles the project or file
)

;; Apply the keybindings when entering c-mode
(add-hook 'c-mode-hook 'setup-c-mode-compile-keybindings)
