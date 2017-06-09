;;; #File and Director settings
(require 'autoinsert)
(require 'user-defines)

(setq dropbox-directory
      (case system-type
        (windows-nt "D:/Dropbox")
        (gnu/linux "~/Dropbox")))

;; #Private
(add-to-list 'load-path (join-path dropbox-directory "src/emacs.d"))
(require 'user-private nil t)

;; #auto-insert
;; depends: yasnippet
(setq auto-insert-directory (join-path dropbox-directory "src/templates/"))

(defun my-autoinsert-yas-expand ()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(defmacro add-yas-auto-insert (condition fname)
  `(add-to-list 'auto-insert-alist (cons ,condition (vector ,fname 'my-autoinsert-yas-expand))))

(defmacro add-yas-auto-insers (&rest reg-file-pairs)
  `(loop for (reg file) in ',reg-file-pairs do
         (add-yas-auto-insert reg file)))

(with-eval-after-load 'yasnippet
  (add-yas-auto-insers
   ("\\.c$" "c")
   ("\\.cc$" "cc")
   ("\\.h$" "h")
   ("\\.html$" "html")
   ("\\.lisp$" "lisp")
   ("Makefile" "Makefile")
   (".projectile" "projectile")
   ("\\.org$" "org")
   ("\\.pl$" "pl")
   ("\\.py$" "py")
   ("\\.sh$" "sh")))

(with-eval-after-load 'migemo
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-coding-system 'utf-8-unix)
  (setq migemo-dictionary
        (case system-type
          (gnu/linux "/usr/share/cmigemo/utf-8/migemo-dict")
          (windows-nt "c:/opt2/cmigemo/dict/utf-8/migemo-dict"))))

(provide 'user-file-dir)
