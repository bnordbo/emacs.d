;; Settings
(setq mac-command-modifier 'meta)
(setq ring-bell-function 'ignore)
(setq-default indent-tabs-mode nil)

;; Modes
(tool-bar-mode -1)
(setq visible-bell t)
(column-number-mode t)

;; Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Emacs server
(server-start)

(provide 'init-editor)
