;; org-export
(defvar org-export-directory nil)

(defun org-export-output-file-name--set-directory (orig-fn extension &optional subtreep pub-dir)
  (setq pub-dir (or pub-dir org-export-directory))
  (funcall orig-fn extension subtreep pub-dir))
(advice-add 'org-export-output-file-name :around 'org-export-output-file-name--set-directory)

(provide 'user-advices)