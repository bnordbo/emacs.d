;; Globally useless keybindings
(global-unset-key (kbd "C-x m"))  ; No sending mail from Emacs

;; Navigation
(defun other-window-back ()
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-<tab>")   'other-window)
(global-set-key (kbd "C-S-<tab>") 'other-window-back)

;; General editing
(global-set-key (kbd "C-x a r")	  'align-regexp)
(global-set-key (kbd "M-<down>")  'md/move-lines-down)
(global-set-key (kbd "M-<up>")    'md/move-lines-up)
(global-set-key (kbd "C-M-<SPC>") 'just-one-space)

;; Magit
(global-set-key (kbd "C-x m s") 'magit-status)

(provide 'init-keys)
