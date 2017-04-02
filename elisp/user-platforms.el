;; #platform settings
(require 'user-defines)

(use-package mozc
  :if (executable-find "mozc_emacs_helper")
  :init (setq default-input-method "japanese-mozc")
  :config (prefer-coding-system 'utf-8-unix))

(with-eval-after-load 'mozc
  (setq mozc-candidate-style 'popup)
  (require 'mozc-popup)

  (global-set-key (kbd "s-o") 'activate-input-method-cmd)
  (global-set-key (kbd "s-i") 'inactivate-input-method-cmd)

  (with-eval-after-load 'evil
    (add-hook 'evil-insert-state-entry-hook 'inactivate-input-method-cmd))

  (when (or (eq system-type 'windows-nt)
            (and (file-exists-p "/proc/version")
                 (not (string= "" (shell-command-to-string "grep Microsoft /proc/version")))))
    ;; Windows の mozc ではセッション接続直後 directモード になるので hiraganaモード にする
    (advice-add
     'mozc-session-execute-command
     :after (lambda (&rest args)
              (when (eq (nth 0 args) 'CreateSession)
                ;; (mozc-session-sendkey '(hiragana)))))
                (mozc-session-sendkey '(Hankaku/Zenkaku)))))))

(defun windows-initialize ()
  (setq default-directory "~/")
  (when window-system
    (set-frame-font "ＭＳ ゴシック-11")) ; (font-candidate "源ノ角ゴシック Code JP R-10" "ＭＳ ゴシック-11")))
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
  ;;(require 'gnutls)
  ;;(setq dynamic-library-alist
  ;;      (let ((res))
  ;;        (dolist (elm dynamic-library-alist res)
  ;;          (if (equal 'gnutls (car elm))
  ;;              (add-to-list 'res '(gnutls "libgnutls-30.dll") t)
  ;;            (add-to-list 'res elm t)))))
  ;;(setq gnutls-trustfiles '("c:/msys64/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem"))
  )

(case system-type
  (windows-nt (windows-initialize)))

(provide 'user-platforms)
