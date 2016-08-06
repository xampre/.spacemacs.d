;;; #File and Director settings
(require 'user-defines)

(setq auto-insert-directory "~/.spacemacs.d/templates")
(with-eval-after-load 'org
  (setq org-return-follows-link t
        org-startup-with-inline-images t
        org-startup-truncated nil
        org-image-actual-width nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")
          (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")))
  (add-hook 'org-mode-hook 'flyspell-mode)
  )

(provide 'user-file-dir)
