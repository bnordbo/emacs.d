;; -*- lexical-binding: t -*-

;; Open a new line below the current one and indent.  I missed this
;; function for ten years.  It took me two minutes to whip together this
;; one.

(defun bn/next-line-and-indent ()
  (interactive)
  (end-of-line)
  (default-indent-new-line))

(defun bn/roam-files ()
  (directory-files "~/Repository/Roam" t ".*org$"))

(defun bn/other-window-back ()
  (interactive)
  (other-window -1))

(defun bn/agenda-files-recursively (dir)
  (directory-files-recursively dir "\.org$" nil
                               (bn/agenda-file-fn dir 1 '("Archive"))))

(defun bn/agenda-file-fn (base-dir max-depth &optional ignore)
  "Returns a predicate that is true if the file passed to it
should be considered by org-agenda. BASE-DIR is the base
directory passed to directory-files-recursively, and MAX-DEPTH is
the max depth to search. If IGNORE is specified, it will return
false if the file matches a name in this list."
  (lambda (file)
    (let ((parts (split-string (replace-regexp-in-string base-dir "" file) "/" t)))
      (and (not (member (car (last parts)) ignore))
           (<= (length parts) max-depth)))))

(defun bn/project-p ()
  (string= (nth 2 (org-heading-components)) "PROJ"))

(defun bn/select-projects ()
  (bn/select-if #'bn/project-p))

(defun bn/select-if (pred)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading)
                                             (point-max)))))
      (unless (funcall pred) next-headline))))

(defvar bn/org-agenda--active-projects
  '(tags-todo "-INACTIVE-SOMEDAY-CANCELLED/!"
              ((org-agenda-overriding-header "Projects")
               (org-agenda-skip-function 'bn/select-projects))))

(defun bn/bms (tags)
  (interactive "sTags: ")
  (org-ql-sparse-tree `(tags ,tags)))

; From org-journal FAQ
(defun bn/org-journal-save-entry-and-exit ()
  "Simple convenience function.
  Saves the buffer of the current day's entry and kills the window
  Similar to org-capture like behavior"
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))

(defun bn/select-theme (name appearence)
  (cdr (assoc appearence (cadr (assoc name theme-sets)))))

(defun bn/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme (bn/select-theme default-theme appearance) t)
  ; Gets reset after loading themes for some reason.
  (bn/adjust-face-attributes))

; It would be nice to be able to quickly search a document and all level-n
; links, e.g. using swiper.
(defun bn/org-buffer-links ()
  (interactive)
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) "file")
        (org-element-property :path link)))))
