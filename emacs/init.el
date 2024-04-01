;; Stops startup message
(setq inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; bell
(setq visible-bell t)

;; linhas
(global-display-line-numbers-mode t)

;; auto brackets
(use-package flex-autopair
  :ensure t)

;; theme
(setq custom-safe-themes t)
(use-package rebecca-theme
  :ensure t
  :config (load-theme 'rebecca))

;; font
(set-face-attribute 'default nil
                    :family "Fira Code Retina"
                    :weight 'bold
                    :height 120)

;; Pacotes
(require 'package)
(setq package-enable-at-startup nil)

;; MELPA
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")) 

(package-initialize) ; iniciar pacotes

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; autocomplete
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))

;; neotree
(use-package neotree
  :ensure t
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :bind (("C-\\" . 'neotree-toggle)))

;; all-the-icons
(use-package all-the-icons
  :ensure t)


;; Golang
(use-package go-mode
  :ensure t)

;; Go-complete
(use-package go-complete
  :ensure t)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (go-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :ensure t)

;; Company mode
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Start LSP Mode and YASnippet mode
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)

;; MARKDOWN
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode 1))

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

;; meus atalhos
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-c c") 'kill-ring-save)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-w") 'kill-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tab
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "TAB") 'tab-to-tab-stop)

(defun indent-region-custom(numSpaces)
  (progn
    ;; Se o texto está selecionado, indentar esse texto
    (if (use-region-p)
        (let ((mark (mark)))
          (save-excursion
            (indent-rigidly (region-beginning) (region-end) numSpaces)
            (push-mark mark t t)
            (setq deactivate-mark nil)))
      ;; Senão, apenas inserir um tab
      (insert-tab))))

(defun my-indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
    (indent-region-custom 4)))

(global-set-key (kbd "TAB") 'my-indent-or-complete)

(defun unindent-region-custom(numSpaces)
  "Reduz a indentação de uma região selecionada por um número específico de espaços."
  (progn
    ;; Verifica se há texto selecionado
    (if (use-region-p)
        (let ((mark (mark)))
          (save-excursion
            ;; Reduz a indentação da região selecionada
            (indent-rigidly (region-beginning) (region-end) (- numSpaces))
            (push-mark mark t t)
            ;; Mantém a seleção após a indentação
            (setq deactivate-mark nil)))
      ;; Se não houver seleção, não faz nada
      )))

(defun my-unindent-or-nothing ()
  "Chama a função de desindentar se houver uma região selecionada, caso contrário, não faz nada."
  (interactive)
  (unindent-region-custom 4))

;; Mapeia Shift + Tab para a função de reduzir indentação
(global-set-key (kbd "<backtab>") 'my-unindent-or-nothing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Se estiver usando 'undo-tree', pode usar:
(global-set-key (kbd "C-y") 'undo-tree-redo)

;; ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-f") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9724b3abaf500b227faa036dcf817abed9764802835ba6e8d1e475c877205157" default))
 '(package-selected-packages
   '(magit counsel helm flex-autopair eletric-pair-mode lsp-ui molokai-theme all-the-icons neotree auto-complete which-key try lsp-mode go-mode go flycheck company))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
