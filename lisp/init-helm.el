(require 'helm-config)

(helm-mode t)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x 4 f") #'find-file-other-window)

;; Use helm-smex for minibuffer commands.
;(global-set-key [remap execute-extended-command] #'helm-smex)
;(global-set-key (kbd "M-X") #'helm-smex-major-mode-commands)

(provide 'init-helm)
