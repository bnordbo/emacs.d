(load-library "go-mode")

(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width 4)))

(provide 'init-go)
