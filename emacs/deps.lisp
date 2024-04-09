(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'asdf))

(defun run-command (command)
  "Executes a command and prints its output in real time."
  (format t "Executing: ~a~%" command)
  (uiop:run-program command :output *standard-output*))

(defun run-command-as-root (command)
  "Executes a command as root and prints its output in real time."
  (format t "Executing as root: ~a~%" command)
  (uiop:run-program (format nil "doas ~a" command) :output *standard-output*))

(defun pkg-installed-p (pkg-name)
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program (format nil "pkg info | grep -q ~A" pkg-name)
                        :output '(:string :stripped t)
                        :error-output '(:string :stripped t)
                        :ignore-error-status t)
    (zerop exit-code)))

(defun install-pkg (pkg-name)
  (unless (pkg-installed-p pkg-name)
    (format t "Checking package installation: ~A~%" pkg-name)
    (run-command-as-root (format nil "pkg install -y ~A" pkg-name))))

(defun npm-pkg-installed-p (pkg-name)
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program (format nil "npm list -g --depth=0 | grep -q ~A" pkg-name)
                        :output '(:string :stripped t)
                        :error-output '(:string :stripped t)
                        :ignore-error-status t)
    (zerop exit-code)))

(defun install-npm-pkg (pkg-name)
  (unless (npm-pkg-installed-p pkg-name)
    (format t "Checking npm package installation: ~A~%" pkg-name)
    (run-command (format nil "npm install -g ~A" pkg-name))))

(install-pkg "openjdk17")
(install-pkg "npm")
(install-npm-pkg "bash-language-server")
(install-pkg "git")
(install-pkg "llvm")

(when (not (uiop:run-program "command -v gopls" :ignore-error-status t))
  (run-command "go install golang.org/x/tools/gopls@latest"))
