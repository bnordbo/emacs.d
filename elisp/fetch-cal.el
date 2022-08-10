(defvar bn/calendars
      '(("WG2" "https://calendar.google.com/calendar/ical/wgtwo.com_aseoqn4noan8ilsjvpi72uv3l8%40group.calendar.google.com/private-fe4e0c1db1256c14db2eb8f6bcc8948a/basic.ics"))
      "List of calendars to fetch. Each entry should be a list of (name url).")

(defvar bn/org-cal-dir "~/.emacs.d/calendars"
      "Directory for storing converted calendars.")

(defvar bn/ics-to-org-command "~/.emacs.d/ical2org.awk")

(defun bn/fetch (name url)
  (let ((ics-path (make-temp-name (format "fetch-cal-%s" name)))
        (org-path (format "%s/%s.org" bn/org-cal-dir name)))
    (message "Fetching %s to %s via %s" name org-path ics-path)
    (url-copy-file url ics-path)
    (bn/ice-to-org name ics-path org-path)))


(defun bn/fetch-all ()
  (dolist (cal bn/calendars)
    (apply 'bn/fetch cal)))

(defun bn/ice-to-org (name ics-file org-file)
  (message "awk -f %s < %s > %s"
           (expand-file-name bn/ics-to-org-command)
           ics-file
           (expand-file-name org-file))
  (let ((process-environment
         (append process-environment
                 `(,(format "CALENDAR=%s" name)
                   "AUTHOR=Bjørn Nordbø"
                   "EMAIL=bn@wgtwo.com"
                   "FILETAGS=@office"))))
    (call-process "awk"
                  ics-file
                  (list :file (expand-file-name org-file))
                  nil
                  "-f" (expand-file-name bn/ics-to-org-command))))

(bn/fetch-all)

(bn/ice-to-org "/Users/bn/.emacs.d/fetch-cal-WG215RbwT" "~/.emacs.d/calendars/WG2.org")

(define-key LaTeX-mode-map "_" (lambda () (interactive) (insert "-")))
(define-key LaTeX-mode-map "-" (lambda () (interactive) (insert "_")))
(define-key LaTeX-mode-map "^" (lambda () (interactive) (insert "6")))
(define-key LaTeX-mode-map "6" (lambda () (interactive) (insert "^")))
