;; Settings
(setq mac-command-modifier 'meta)
(setq ring-bell-function 'ignore)
(setq-default indent-tabs-mode nil)
(setq backup-inhibited t)

;; Modes
(column-number-mode t)
(ido-yes-or-no-mode t)
(show-paren-mode t)

;; Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Emacs server
(server-start)

(provide 'init-editor)
