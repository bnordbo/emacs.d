;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Extra libraries
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "~/src/elisp"))
(load-library "keywiz")

;; Environment
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(require 'init-melpa)
(require 'init-visual)
(require 'init-editor)
(require 'init-ido)
(require 'init-keys)
(require 'init-git)
(require 'init-theme)

(require 'init-go)
(require 'init-haskell)
(require 'init-java)
(require 'init-rust)
