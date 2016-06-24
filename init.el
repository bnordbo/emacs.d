;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "~/src/elisp"))

(add-to-list 'exec-path "/usr/local/bin")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(load-library "keywiz")

(require 'init-melpa)
(require 'init-visual)
(require 'init-editor)
(require 'init-haskell)
(require 'init-ido)
(require 'init-java)
(require 'init-keys)
(require 'init-git)
(require 'init-rust)
(require 'init-theme)
