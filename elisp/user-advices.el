;; org-export
(defvar org-export-directory nil)

(defun org-export-output-file-name--set-directory (orig-fn extension &optional subtreep pub-dir)
  (setq pub-dir (or pub-dir org-export-directory))
  (funcall orig-fn extension subtreep pub-dir))
(advice-add 'org-export-output-file-name :around 'org-export-output-file-name--set-directory)

;; dired-delete-file
(defun after-trim-file-name-history (&rest args) (trim-file-name-history))
(advice-add 'dired-delete-file :after #'after-trim-file-name-history)

;; org-open-at-point
(defun orgTZA-open-at-point-ad (oldfun &rest args)
  "Just `org-open-at-point'.
If region is active open all links in region."
  (if (use-region-p)
      (save-excursion
        (save-restriction
          (narrow-to-region (region-beginning) (region-end))
          (goto-char (point-min))
          (while (progn
                   (org-next-link)
                   (null org-link-search-failed))
            (apply oldfun args))))
    (apply oldfun args)))

(advice-add #'org-open-at-point :around #'orgTZA-open-at-point-ad)

(provide 'user-advices)
