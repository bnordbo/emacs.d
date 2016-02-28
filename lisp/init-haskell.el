(add-hook 'haskell-mode-hook
          (lambda ()
            (subword-mode)
            (electric-pair-mode)
            (interactive-haskell-mode)
            (haskell-doc-mode)
            (subword-mode)))

(provide 'init-haskell)
