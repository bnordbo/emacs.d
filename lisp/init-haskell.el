(add-hook 'haskell-mode-hook
          (lambda ()
            (subword-mode)
            (electric-pair-local-mode)
            (haskell-doc-mode)
            (subword-mode)
            (intero-mode)))

(provide 'init-haskell)
