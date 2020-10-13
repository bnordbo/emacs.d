;; Open a new line below the current one and indent.  I missed this
;; function for ten years.  It took me two minutes to whip together this
;; one.

(defun bn/next-line-and-indent ()
  (interactive)
  (end-of-line)
  (default-indent-new-line))

(defun bn/roam-files ()
  (directory-files "~/Repository/Roam" t ".*org$"))

(defun other-window-back ()
  (interactive)
  (other-window -1))
