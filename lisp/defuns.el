(defun org-ref-here ()
  "Get the string to this file and line"
  (interactive)
  (kill-new (concat "file:"
                    (buffer-file-name)
                    "::"
                    (number-to-string (line-number-at-pos)))))

(provide 'defuns)
