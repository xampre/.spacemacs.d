;; #platform settings
(require 'user-defines)

(defun windows-initialize ()
  (require 'gnutls)
  (setq default-directory "~/")
  (when window-system (set-frame-font "ＭＳ ゴシック-11")) ; (font-candidate "源ノ角ゴシック Code JP R-10" "ＭＳ ゴシック-11")))
  (set-file-name-coding-system 'cp932)
  (set-keyboard-coding-system 'cp932)
  (set-terminal-coding-system 'cp932)
  (prefer-coding-system 'utf-8-unix)
  ;; IME
  (when (fboundp 'w32-ime-initialize)
    (setq default-input-method "W32-IME")
    (setq-default w32-ime-mode-line-state-indicator "[--]")
    (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
    (w32-ime-initialize))
  ;; gnutls
  ;; http://proglab.blog.fc2.com/blog-entry-14.html
  (setq dynamic-library-alist
        (let ((res))
          (dolist (elm dynamic-library-alist res)
            (if (equal 'gnutls (car elm))
                (add-to-list 'res '(gnutls "libgnutls-30.dll") t)
              (add-to-list 'res elm t)))))
  (setq gnutls-trustfiles '("c:/msys64/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem"))
  )

(defun linux-initialize ()
  (when window-system
    (let ((font (font-candidate "Source Code Pro Medium Italic 11")))
      (when font (set-frame-font font))))

  (use-package mozc
    :init (setq default-input-method "japanese-mozc")
    :config (prefer-coding-system 'utf-8-unix))
  (global-set-key (kbd "s-o") 'activate-input-method-cmd)
  (global-set-key (kbd "s-i") 'inactivate-input-method-cmd)
  (with-eval-after-load 'evil
    (add-hook 'evil-insert-state-entry-hook 'inactivate-input-method-cmd)))

(case system-type
  (windows-nt (windows-initialize))
  (gnu/linux (linux-initialize))
  (t nil))

(provide 'user-platforms)
