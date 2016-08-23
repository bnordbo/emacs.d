(load-library "company")

(setq racer-cmd "~/bin/racer"
      racer-rust-src-path "~/src/thirdparty/rust/src")

(add-hook 'rust-mode-hook
          (lambda ()
            (electric-pair-mode 1)
            (racer-mode 1)))

(add-hook 'racer-mode-hook
          (lambda ()
            (eldoc-mode 1)
            (company-mode 1)))

(provide 'init-rust)