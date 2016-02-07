(add-hook 'magit-mode-hook (lambda () (local-unset-key (kbd "C-<tab>"))))

(provide 'init-git)
