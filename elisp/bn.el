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
