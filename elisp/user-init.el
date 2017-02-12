(defalias 'yes-or-no-p 'y-or-n-p)
(put 'narrow-to-region 'disabled nil)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(modify-syntax-entry ?_ "w" (standard-syntax-table))
(modify-syntax-entry ?. "w" minibuffer-inactive-mode-syntax-table)
(modify-syntax-entry ?- "w" minibuffer-inactive-mode-syntax-table)
(modify-coding-system-alist 'file "\\.ahk\\'" 'shift_jis-dos)
(prefer-coding-system 'utf-8-unix)
(set-language-environment "Japanese")
(add-hook 'text-mode-hook 'flyspell-mode)


;; #setqs
(setq confirm-kill-emacs 'y-or-n-p)
(setq next-screen-context-lines 1)
(setq linum-format "%4d")
(setq auto-insert-query nil)
(setq delete-by-moving-to-trash t)
(setq compilation-scroll-output t)
(setq scroll-conservatively 1)
;;(setq line-spacing 0.1)
(setq whitespace-display-mappings
      ;; show whitespace
      '((space-mark ?\u3000 [?\u25a1])
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
        (tab-mark ?\t [?\xBB ?\t] [?\\ ?\t])))
(setq eldoc-idle-delay 1
      eldoc-echo-area-use-multiline-p t)
(setq vr/engine 'pcre2el) ; visual-regexp-steroids
(setq desktop-missing-file-warning nil)
(setq helm-ag-base-command "grep -rn")
(setq flycheck-pos-tip-timeout 20)
;;(setq recentf-max-saved-items 500
;;      recentf-auto-cleanup 10
;;      recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))

;; #enable modes
(desktop-save-mode t)

;; #disable modes
(electric-indent-mode -1)

(provide 'user-init)
