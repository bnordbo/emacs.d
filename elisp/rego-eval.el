(setq bn/chordlet-rego-lib-dir "/Users/bn/Contexts/WG2/src/loltel/voice/chordlet/assets/opa/lib/")

(defun bn/eval-query ()
  (interactive)
  (save-some-buffers)
  (let ((out (get-buffer-create "*rego-eval*")))
    (with-current-buffer out
      (read-only-mode 0)
      (delete-region (point-min) (point-max)))
    (bn/run-eval (bn/input-file) (bn/data-file) (read-from-minibuffer "Query: ") out)
    (view-buffer out)))

(defun bn/run-eval (input-file data-file query out)
  (call-process-shell-command
   (format "opa eval --explain fails -f pretty -i %s -d %s -d %s '%s'"
           input-file bn/chordlet-rego-lib-dir data-file query)
   nil '("*rego-eval*" t) t))

(defun bn/input-file ()
  (bn/derive-file-name "json"))

(defun bn/data-file ()
  (bn/derive-file-name "rego"))

(defun bn/derive-file-name (ext)
  (f-swap-ext (buffer-file-name) ext))

(defun bn/query-at-point ()
  (save-excursion
    (let ((orig-pos (point)))
      (string-trim
       (buffer-substring-no-properties
        (bn/begin-pos orig-pos) (bn/end-pos orig-pos))))))

(defun bn/begin-pos (from)
  (bn/empty-line-pos from #'re-search-backward))

(defun bn/end-pos (from)
  (bn/empty-line-pos from #'re-search-forward))

(defun bn/empty-line-pos (from search-fn)
  (goto-char from)
  (funcall search-fn "\n\s*\n" nil 'pos)
  (point))
