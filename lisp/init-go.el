(load-library "go-mode")

(add-hook 'go-mode-hook
          (lambda ()
            (electric-pair-mode t)
            (setq tab-width 4)))

(provide 'init-go)
