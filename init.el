;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Environment
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;(require 'exec-path-from-shell)
;(exec-path-from-shell-initialize)

;; Extra libraries
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "~/src/elisp"))

(load-library "keywiz")

(require 'init-bn)

(require 'init-melpa)

(require 'init-editor)
(require 'init-git)
;(require 'init-ido)
(require 'init-helm)
(require 'init-ispell)
(require 'init-keys)
(require 'init-org)
(require 'init-theme)
(require 'init-visual)

(require 'init-go)
(require 'init-haskell)
(require 'init-java)
(require 'init-rust)

;; Third party libraries with local sources
(add-to-list 'load-path (expand-file-name "~/src/elisp/intero/elisp"))

(load-library "intero")
