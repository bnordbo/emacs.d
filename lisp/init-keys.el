;; General key setup
(setq mac-option-modifier nil)

;; Globally useless keybindings
(global-unset-key (kbd "C-x m"))  ; No sending mail from Emacs

;; Navigation
(defun other-window-back ()
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-<tab>")   'other-window-back)
(global-set-key (kbd "C-S-<tab>") 'other-window)

(global-set-key (kbd "S-<tab>") 'idomenu)

;; General editing
(global-set-key (kbd "C-x \\")	  'align-regexp)
(global-set-key (kbd "M-<down>")  'md/move-lines-down)
(global-set-key (kbd "M-<up>")    'md/move-lines-up)
(global-set-key (kbd "C-M-<SPC>") 'just-one-space)
(global-set-key (kbd "C-M-<return>") 'bn/smart-semicolon)
(global-set-key (kbd "C-x C-d") 'md/duplicate-down)

;; Corral
(global-set-key (kbd "M-9") 'corral-parentheses-backward)
(global-set-key (kbd "M-0") 'corral-parentheses-forward)
(global-set-key (kbd "M-[") 'corral-brackets-backward)
(global-set-key (kbd "M-]") 'corral-brackets-forward)
(global-set-key (kbd "M-{") 'corral-braces-backward)
(global-set-key (kbd "M-}") 'corral-braces-forward)
(global-set-key (kbd "M-\"") 'corral-double-quotes-backward)

;; Visual regexp
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)

;; Magit
(global-set-key (kbd "C-x m s") 'magit-status)

;; Smex
(global-set-key (kbd "M-x") 'smex)

(provide 'init-keys)
