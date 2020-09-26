(load-library "go-mode")

(add-hook 'go-mode-hook
          (lambda ()
            (go-eldoc-setup)
            (add-hook 'before-save-hook 'gofmt-before-save)
            (local-set-key (kbd "M-.") 'godef-jump)
            (auto-highlight-symbol-mode 1)
            (electric-pair-local-mode 1)
            (subword-mode 1)
            (yas-minor-mode 1)
            (setq tab-width 4
                  go-eldoc-gocode "/Users/bn/go/bin/gocode")))

(provide 'init-go)
