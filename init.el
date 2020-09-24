(require 'package)
(package-initialize)

(org-babel-load-file "~/.emacs.d/config.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(key-quiz zoom yaml-mode ws-butler whole-line-or-region visual-regexp-steroids use-package toml-mode terraform-mode solarized-theme rust-playground reverse-theme racer protobuf-mode pastelmac-theme ox-gfm org-pomodoro org-bullets org-attach-screenshot org move-dup memoize magit-popup lsp-rust kotlin-mode idomenu hydandata-light-theme hyai helm-tramp helm-smex helm-rtags helm-rg helm-org helm-c-yasnippet hc-zenburn-theme haskell-mode green-screen-theme graphviz-dot-mode go-playground go-fill-struct go-eldoc go-autocomplete go-add-tags github-pullrequest flycheck exec-path-from-shell eshell-autojump eproject edts dumb-jump deft corral company cargo buffer-move browse-at-remote bazel-mode autothemer alect-themes ace-jump-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
