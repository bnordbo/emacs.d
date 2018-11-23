(add-hook 'org-mode-hook
          (lambda ()
            (electric-pair-local-mode -1)
            (local-unset-key (kbd "C-<tab>"))))

(provide 'init-org)
