;; Settings
(setq mac-command-modifier 'meta)
(setq ring-bell-function 'ignore)
(setq-default indent-tabs-mode nil)
(setq backup-inhibited t)
(setq auto-save-visited-interval 45)

;; Modes
(column-number-mode t)
(show-paren-mode t)
(electric-pair-mode t)
(global-move-dup-mode t)
(auto-save-visited-mode t)

;; Whitespace management
(add-hook 'find-file-hook #'ws-butler-global-mode)
(setq require-final-newline t)

;; Make align-regexp always use spaces
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

;; Extra modes
(add-to-list 'auto-mode-alist '("BUILD\\'" . bazel-mode))

;; Auto-highlighing settings
(setq ahs-idle-interval 1.0
      ahs-default-range 'ahs-range-whole-buffer
      ahs-inhibit-face-list '(font-lock-comment-delimiter-face
                              font-lock-comment-face
                              font-lock-doc-face
                              font-lock-doc-string-face
                              font-lock-string-face))

;; Commands
(setq markdown-open-command "~/bin/mark")

;; Open a new line below the current one and indent
(defun bn-next-line-and-indent ()
  (interactive)
  (end-of-line)
  (default-indent-new-line))
(global-set-key (kbd "C-<return>") 'bn-next-line-and-indent)

;; Emacs server
(server-start)

(provide 'init-editor)
