;; Ensure use-package is available
(require 'use-package)

;; lsp-mode setup for Java development
(use-package lsp-mode
	:ensure t
	:commands (lsp lsp-deferred)
	:hook (java-mode . lsp-deferred)
	:init
	(setq lsp-enable-file-watchers nil)
	:config
	;; Specify the path to the language server jar file or script
	;; This needs to be adjusted according to your specific lsp server setup for Java
	(setq lsp-java-jdt-download-url "http://download.eclipse.org/jdtls/milestones/latest/")
	;; Optional: Specify workspace directory
	(setq lsp-java-workspace-dir "~/.emacs.d/workspace/")
	;; Optional: If you want to use lombok, specify the path to the lombok jar
	(setq lsp-java-vmargs
				(list "-noverify"
							"-Xmx2G"
							"-XX:+UseG1GC"
							"-XX:+UseStringDeduplication"
							(concat "-javaagent:" (expand-file-name "~/.emacs.d/lombok.jar"))
							(concat "-Xbootclasspath/a:" (expand-file-name "~/.emacs.d/lombok.jar"))))
	(setq lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
	(setq lsp-java-format-settings-profile "GoogleStyle")
	;; Enable/disable features
	(setq lsp-java-save-actions-organize-imports t)
	(setq lsp-java-autobuild-enabled t))

;; lsp-ui for better UI
(use-package lsp-ui
	:ensure t
	:after lsp-mode
	:commands lsp-ui-mode
	:config
	(setq lsp-ui-doc-enable t
				lsp-ui-doc-position 'top
				lsp-ui-doc-show-with-cursor t
				lsp-ui-doc-show-with-mouse nil
				lsp-ui-sideline-enable t
				lsp-ui-sideline-show-hover t
				lsp-ui-sideline-show-code-actions t))
