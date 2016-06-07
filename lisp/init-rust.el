(add-hook 'rust-mode-hook
          (lambda ()
            (electric-pair-mode 1)))

(provide 'init-rust)
