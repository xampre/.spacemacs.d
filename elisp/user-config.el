(require 'user-defines)
(require 'user-file-dir)
(require 'user-langs)
(require 'user-platforms)
(require 'user-advices)

(require 'autoinsert)
(require 'whitespace)
(require 'evil)
(require 'helm-config)

;; #helm
;; depends: yasnippet helm-yas-complete
(defun open-org-file ()
  "List all files in org-directory"
  (interactive)
  (helm :sources
        `((name . "org files list")
          (candidates . (lambda () (directory-files org-directory nil "[^(..?)]")))
          (candidate-number-limit . 100)
          (action . (("Open" . (lambda (x) (find-file (expand-file-name x org-directory))))
                     ("Other window" . (lambda (x) (find-file-other-window (expand-file-name x org-directory)))))))))

(defun yas-expand-or-helm ()
  (interactive)
  (let ((yas-fallback-behavior 'return-nil))
    (or (yas-expand) (helm-yas-complete))))

;; #auto-insert
;; depends: yasnippet
(defun my-autoinsert-yas-expand ()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(defmacro add-yas-auto-insert (condition fname)
  `(add-to-list 'auto-insert-alist (cons ,condition (vector ,fname 'my-autoinsert-yas-expand))))

(defmacro add-yas-auto-insers (&rest reg-file-pairs)
  `(loop for (reg file) in ',reg-file-pairs do
        (add-yas-auto-insert reg file)))

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
 ("\\.sh$" "sh"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #define keys
;;(ffap-bindings)
;;(windmove-default-keybindings) ; Shift + ↓ or → or ← or ↑ でウィンドウバッファの移動
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-c:" 'uncomment-region)
(global-set-key "\C-c;" 'comment-region)
(global-set-key "\C-c\C-r" 'org-reveal)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ci" 'ibuffer)
(global-set-key "\C-cv" 'view-mode)
(global-set-key "\C-xK" 'kill-buffer-and-window)
(global-set-key "\C-xk" 'kill-this-buffer)
(global-set-key "\M-\C-r" 'vr/isearch-backward)
(global-set-key "\M-\C-s" 'vr/isearch-forward)
(global-set-key "\M-\C-y" 'lifo)
(global-set-key (kbd "M-%") 'vr/query-replace)
(global-set-key [(f12)] 'bs-cycle-next)
(global-set-key [C-M-down] 'open-next-file)
(global-set-key [C-M-up] 'open-previous-file)
(global-set-key [C-f8] 'run-konsole-this-buffer-dir)
(global-set-key [C-iso-lefttab] 'mode-line-other-buffer)
(global-set-key [C-return] 'find-file-at-point)
(global-set-key [C-tab] 'mode-line-other-buffer)
(global-set-key [M-down] 'move-line-down)
(global-set-key [M-up] 'move-line-up)
(global-set-key [S-f12] 'bs-cycle-previous)
(global-set-key "\C-z" 'winner-undo)
(global-set-key (kbd "C-S-Z") 'winner-redo)
(global-set-key (kbd "M-[") 'hs-toggle-hiding)
(global-set-key (kbd "M-]") 'hs-hide-all)

(define-key evil-motion-state-map "\C-z" nil)
(define-key evil-motion-state-map (kbd "C-S-z") nil)
;; minibuffer
(define-key minibuffer-local-completion-map "\C-w" 'backward-kill-word)
(define-key minibuffer-local-filename-completion-map " " 'minibuffer-complete-word)

;; evil
(setcdr evil-insert-state-map nil) ; 挿入ステートでの キーバインドを 完全に Emacs互換にする
(define-key evil-insert-state-map [escape] 'evil-normal-state)

(evil-swap-key evil-motion-state-map "j" "gj")
(evil-swap-key evil-motion-state-map "k" "gk")
;; insert state
(define-key evil-insert-state-map "\C-w" 'backward-kill-word)
(define-key evil-insert-state-map (kbd "C-S-w") 'kill-whole-line)
(define-key evil-insert-state-map "\C-cu" 'backward-kill-line)
(define-key evil-insert-state-map (kbd "C-,") 'company-complete)
(define-key evil-insert-state-map (kbd "C-.") 'yas-expand-or-helm)
;; motion-state
(unless (eq 'spacemacs-cmds (lookup-key evil-motion-state-map " "))
  ;; define prefix key
  (define-key evil-motion-state-map " " nil))
(define-key evil-motion-state-map "  " 'helm-M-x)
(define-key evil-motion-state-map " b" 'helm-buffers-list)
(define-key evil-normal-state-map " d" 'duplicate-current-line)
(define-key evil-motion-state-map " o" 'helm-bookmarks)
(define-key evil-motion-state-map "t" 'avy-migemo-goto-word-1)
(define-key evil-motion-state-map " Ha" 'hs-show-all)
(define-key evil-motion-state-map " HA" 'hs-hide-all)
(define-key evil-motion-state-map " Hb" 'hs-show-block)
(define-key evil-motion-state-map " HB" 'hs-hide-block)
(define-key evil-motion-state-map " HL" 'hs-hide-level)
(define-key evil-motion-state-map " Ht" 'hs-toggle-hiding)

;; dired
(put 'dired-find-alternate-file 'disabled nil)
(with-eval-after-load 'dired-x
  (define-key dired-mode-map [f5] 'revert-buffer)
  (define-key dired-mode-map [tab] 'other-window-or-split)
  (define-key dired-mode-map [backspace] 'dired-up-directory)
  (define-key dired-mode-map [return] 'dired-find-alternate-file)
  (define-key dired-mode-map "a" 'dired-find-file)
  (define-key dired-mode-map "b" 'dired-up-directory)
  (define-key dired-mode-map "h" 'dired-up-directory)
  (define-key dired-mode-map "l" 'dired-find-alternate-file)
  (define-key dired-mode-map "w" 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map "T" 'dired-sort-time)
  (define-key dired-mode-map "F" 'dired-sort-name))

;; Mouse
(global-set-key [wheel-up] 'scroll-down-with-lines)
(global-set-key [wheel-down] 'scroll-up-with-lines)
(global-set-key [mouse-3] 'mouse-popup-menubar)
(global-set-key [C-wheel-down] '(lambda () (interactive) (text-scale-increase -1)))
(global-set-key [C-wheel-up] '(lambda () (interactive) (text-scale-increase 1)))
(global-set-key [C-mouse-2] '(lambda () (interactive) (text-scale-increase 0)))

(with-eval-after-load 'whitespace
  (set-face-attribute 'whitespace-tab nil :background nil :underline  t)
  (set-face-attribute 'trailing-whitespace nil :underline t :background nil))

(with-eval-after-load 'org
  (setq org-return-follows-link t
        org-startup-with-inline-images t
        org-startup-truncated nil
        org-image-actual-width nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")
          (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")))
  (add-hook 'org-mode-hook 'flyspell-mode)
  (define-key org-mode-map [C-tab] 'mode-line-other-buffer)
  (define-key org-mode-map [f5] 'org-mode-restart)
  (define-key org-mode-map [C-f5] 'org-render-html)
  )

(with-eval-after-load 'desktop
  (delete 'file-name-history desktop-globals-to-save)
  (delete 'tags-file-name desktop-globals-to-save)
  (delete 'tags-table-list desktop-globals-to-save))

(with-eval-after-load 'projectile
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "bower_modules"))

(with-eval-after-load 'helm
  (define-key helm-map "\C-w" 'backward-kill-word))

(with-eval-after-load 'yasnippet
  (add-hook 'snippet-mode-hook 'do-not-want-final-newline)
  (define-key yas-minor-mode-map [tab] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil))

(with-eval-after-load 'company
  (define-key company-active-map "\C-n" 'company-select-next)
  (define-key company-active-map "\C-p" 'company-select-previous)
  (define-key company-active-map "\C-n" 'company-select-next)
  (define-key company-active-map [backtab] 'company-select-previous))

(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map [f2] 'flycheck-next-error)
  (define-key flycheck-mode-map [S-f2] 'flycheck-previous-error))

(with-eval-after-load 'savehist
  (setq history-length 500))

;; #Private
(add-to-list 'load-path "~/.emacs.d/private/elisp")
(require 'user-private nil t)

;; #advices
(defun after-trim-file-name-history (&rest args) (trim-file-name-history))
(advice-add 'dired-delete-file :after #'after-trim-file-name-history)

;; #hooks
(add-hook 'after-save-hook 'delete-file-if-no-contents)

;; #enable modes
(auto-image-file-mode t)
(global-whitespace-mode t)
(global-auto-revert-mode t)
(delete-selection-mode t)
(show-paren-mode t)
(auto-insert-mode t)
;; elpa
(global-flycheck-mode t)
(flycheck-pos-tip-mode t)
(global-company-mode t)
(helm-descbinds-mode t)

(provide 'user-config)
