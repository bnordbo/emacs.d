(defun bn/smart-semicolon ()
  (interactive)
  (end-of-line)
  (when (not (looking-back ";"))
    (insert ";")
    (newline-and-indent)))

(provide 'init-bn)
