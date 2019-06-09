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
            (local-unset-key (kbd "C-<tab>"))))

(setq org-directory "~/src/org"
      org-default-notes-file (o-file "inbox")
      org-agenda-restore-windows-after-quit t
      org-log-done t)

(setq org-agenda-files (directory-files-recursively "~/src/org" "org$"))

(setq org-refile-targets `((,(o-file "gtd") :maxlevel . 3)
                           (,(o-file "someday") :level . 1)
                           (,(o-file "tickler") :maxlevel . 2)))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-agenda-custom-commands
      '(("o" "At the Oslo office" tags-todo "@oslo"
         ((org-agenda-overriding-header "Oslo office")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
        ("h" "At the home office" tags-todo "@home"
         ((org-agenda-overriding-header "Home office")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
        ("x" "Offline tasks" tags-todo "@offline"
         ((org-agenda-overriding-header "Offline")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))

(setq org-capture-templates
      `(("t" "TODO" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %i%?")
        ("T" "Tickler" entry (file+headline ,(o-file "tickler") "Tickler")
         "*  %i%? \n %U")
        ("n" "Note" entry (file org-default-notes-file)
         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("d" "Diary" entry (file+datetree ,(o-file "diary"))
         "* %?\n%U\n" :clock-in t :clock-resume t)
        ("m" "Meeting" entry (file org-default-notes-file)
         "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)))

(provide 'init-org)
