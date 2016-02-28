;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-visual)
(require 'init-editor)
(require 'init-haskell)
(require 'init-ido)
(require 'init-java)
(require 'init-keys)
(require 'init-git)
(require 'init-melpa)
(require 'init-theme)
