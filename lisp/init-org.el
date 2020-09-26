(defun o-file (n)
  (format "%s/%s.org" org-directory n))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(add-hook 'org-mode-hook
          (lambda ()
            (electric-pair-local-mode -1)
            (auto-fill-mode t)
            (local-unset-key (kbd "C-<tab>"))
            (local-unset-key (kbd "M-h"))
            (local-set-key (kbd "M-S-<up>") 'org-move-subtree-up)
            (local-set-key (kbd "M-S-<down>") 'org-move-subtree-down)))

(add-hook 'auto-save-hook 'org-save-all-org-buffers)

(setq org-directory "~/src/org"
      org-default-notes-file (o-file "inbox")
      org-agenda-restore-windows-after-quit t
      org-agenda-window-setup 'current-window
      org-log-done t)

;; Refile settings.  Allow refiling to top-level headings.  I added
;; this because I wanted to be able to refile directly to a new
;; top-level project.  In practice it does not work great because the
;; list becomes huge and messy, a cache is "needed" and causes stale
;; reads, and usually the project will be different from the item
;; anyway.
;;
;; (setq org-refile-use-outline-path 'file
;;       org-outline-path-complete-in-steps nil
;;       org-refile-allow-creating-parent-nodes 'confirm
;;       org-refile-use-cache nil)
;;
;; This can be used to periodically update the cache, if enabled.
;; (run-with-idle-timer 300 t (lambda ()
;;                             (org-refile-cache-clear)
;;                             (org-refile-get-targets)))

(setq org-agenda-files `(,(o-file "inbox")
                         ,(o-file "gtd")
                         ,(o-file "tickler")))

(setq org-refile-targets `((,(o-file "gtd") :maxlevel . 3)
                           (,(o-file "someday") :level . 1)
                           (,(o-file "tickler") :maxlevel . 2)
                           (,(o-file "meetings") :level . 1)))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; (setq org-tag-alist
;;      '(("@office" . "o")
;;        ("@reading" . "r")
;;        ("@home" . "h")
;;        ("@project" . "p")))

;; (setq org-agenda-custom-commands
;;       `(("o" "At the office" tags-todo "@office"
;;          ((org-agenda-overriding-header "Oslo office")
;;           (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
;;         ("x" "Offline tasks" tags-todo "@offline"
;;          ((org-agenda-overriding-header "Offline")
;;           (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
;;         ("W" "Weekly review"
;;          ((agenda "" ((org-agenda-span 7)))
;;           (stuck "")
;;           (todo "WAITING")
;;           ; Doesn't work.
;;           (org-agenda-files ,(o-file "someday"))))))

(setq org-agenda-custom-commands
      '(("r" "Review"
         ((agenda)
          (stuck "")
          (tags-todo "@office")
          (tags-todo "@reading")
          (tags-todo "@home")
          (tags-todo "@project")))
        ("D" "Daily action list"
         ((agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up) )))
                      (org-deadline-warning-days 0)))))))

(setq org-capture-templates
      `(("t" "TODO" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %i%?")
        ("T" "Tickler" entry (file+headline ,(o-file "tickler") "Tickler")
         "* TODO %i%? \n %U")
        ("n" "Note" entry (file org-default-notes-file)
         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("d" "Diary" entry (file+datetree ,(o-file "diary"))
         "* %?\n%U\n" :clock-in t :clock-resume t)
        ("m" "Meeting" entry (file org-default-notes-file)
         "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)))

(provide 'init-org)

(defun +org-search ()
  (interactive)
  (org-refile '(4)))
