(require 'f)

;; #Python
;; Set python-shell-interpreter
(defun set-my-python-shell-interpreter ()
  (let* ((py (or (executable-find "ipython3")
                 (executable-find "ipython"))))
    (when py (setq python-shell-interpreter py))))

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(setq-default
 js2-basic-offset 2
 css-indent-offset 2
 web-mode-markup-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-attr-indent-offset 2
 coffee-tab-width 4)

(defun set-js-indent-2 () (setq js-indent-level 2))
(add-hook 'json-mode-hook 'set-js-indent-2)

;; #skewer-mode
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

(defun set-tab-width-4 () (setq tab-width 4))
(add-hook 'makefile-mode-hook 'set-tab-width-4)

(provide 'user-langs)
