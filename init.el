;; First we initialise package and set up the package archies.
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Second we check that use-package is installed.  If not, it is
;; likely a fresh installation, so refresh the packages before
;; installing it.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; Finally we load the literate configuration.
(org-babel-load-file "~/.emacs.d/config.org")

(let ((path (file-name-concat "~/.emacs.d" (system-name) "config.org")))
  (if (file-exists-p path)
      (message "Loading %s" path)
    (message "No host-specific config in '%s' – skipping" path)))
