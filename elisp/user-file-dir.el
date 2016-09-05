;;; #File and Director settings

(setq auto-insert-directory "~/.spacemacs.d/templates")

(use-package migemo
  :if (executable-find "cmigemo")
  :config
  (progn
    (setq migemo-command "cmigemo")
    (setq migemo-options '("-q" "--emacs"))
    (setq migemo-coding-system 'utf-8-unix)
    (setq migemo-dictionary
          (case system-type
            (gnu/linux "/usr/share/cmigemo/utf-8/migemo-dict")
            (windows-nt "c:/opt2/cmigemo/dict/utf-8/migemo-dict")))
    (migemo-init)))

(provide 'user-file-dir)
