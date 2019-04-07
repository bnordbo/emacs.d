(add-hook 'org-mode-hook
          (lambda ()
            (electric-pair-local-mode -1)
            (local-unset-key (kbd "C-<tab>"))))

(defun o-file (n)
  (format "%s/%s.org" org-directory n))

(setq org-directory "~/src/org"
      org-default-notes-file (o-file "refile")
      org-agenda-restore-windows-after-quit t
      org-log-done t)

(setq org-agenda-files (directory-files-recursively "~/src/org" "org$"))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)

(setq org-capture-templates
      `(("t" "todo" entry (file ,(o-file "refile"))
         "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
        ("n" "note" entry (file ,(o-file "refile"))
         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("j" "Journal" entry (file+datetree ,(o-file "diary"))
         "* %?\n%U\n" :clock-in t :clock-resume t)
        ("w" "org-protocol" entry (file ,(o-file "refile"))
         "* TODO Review %c\n%U\n" :immediate-finish t)
        ("m" "Meeting" entry (file ,(o-file "refile"))
         "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
        ("p" "Phone call" entry (file ,(o-file "refile"))
         "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
        ("h" "Habit" entry (file ,(o-file "refile"))
         "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))

(provide 'init-org)
