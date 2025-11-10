(use-package bazel
  :init (setq bazel-command '("bazelisk"))
  :bind
  (("C-c b f b" . bazel-find-build-file)
   ("C-c b t p" . bazel-test-at-point)))

(use-package rego-mode
  :bind (:map rego-mode-map
              ("C-c r e" . bn/eval-query))
  :init
  (add-hook 'rego-mode-hook (lambda ()
                              (setq tab-width 4)))
  :custom
  (rego-repl-executable "/etc/profiles/per-user/bn/bin/opa")
  (rego-opa-command "/etc/profiles/per-user/bn/bin/opa"))
