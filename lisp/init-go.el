(load-library "go-mode")

(add-hook 'go-mode-hook
          (lambda ()
            (electric-pair-local-mode 1)
            (subword-mode 1)
            (setq tab-width 4)))

(provide 'init-go)
