(require 'cl)
(require 'dash)
(require 'f)
(require 's)

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

(defun create-scratch-buffer ()
  "create a scratch buffer"
  (interactive)
  (let ((bn "*scratch*"))
    (cond ((get-buffer bn) (switch-to-buffer bn))
          (t (switch-to-buffer (get-buffer-create bn))
             (lisp-interaction-mode)
             (insert initial-scratch-message)))))

(defun trim-file-name-history ()
  (interactive)
  (setq file-name-history (-filter #'f-exists-p file-name-history)))

(defun do-not-wont-final-newline ()
  (set (make-local-variable 'require-final-newline) nil))

(defun join-path (&rest paths)
  (reduce #'(lambda (x y) (concat (file-name-as-directory x) y)) paths))

(defun f-path-match? (regexp)
  (string-match regexp (or (f-this-file) "")))

(defun pick-random-element (list-var)
  (nth (random (length list-var)) list-var))

(defun add-to-load-path-with-subdirs (path)
  "add path and it's subdirs to load-path"
  (add-to-list 'load-path path)
  (let ((default-directory path))
    (normal-top-level-add-subdirs-to-load-path)))

(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(defun set-alpha (n)
  "set frame parameter 'alpha"
  (interactive
   (list (read-number "Alpha: " (case (frame-parameter nil 'alpha) ((nil 100) 85) (t 100)))))
  (set-frame-parameter nil 'alpha n))

(defun activate-input-method-cmd ()
  ;; activate input method
  (interactive)
  (unless current-input-method (activate-input-method default-input-method)))

(defun inactivate-input-method-cmd ()
  ;; inactivate input method
  (interactive)
  (when current-input-method (deactivate-input-method)))

(defun lifo ()
  (interactive)
  (if kill-ring
      (insert (pop kill-ring))
      (message "Kill ring is empty")))

(defun duplicate-current-line (&optional n)
  "duplicate current line, make more than 1 copy given a numeric argument"
  (interactive "p")
  (save-excursion
    (let ((nb (or n 1))
          (current-line (thing-at-point 'line)))
      ;; when on last line, insert a newline first
      (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
        (insert "\n"))

      ;; now insert as many time as requested
      (while (> n 0)
        (insert current-line)
        (decf n)))))

;;; http://emacswiki.org/emacs/ToggleWindowSplit
(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.
i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

(defun f-dotname? (path)
  (s-equals? "." (s-left 1 (f-filename path))))

(defun* nth-from-elem (elem list-var N &optional (test #'eq))
  (cond ((eq nil (cdr list-var)) nil)
        ((funcall test elem (car list-var)) (nth N list-var))
        (t (nth-from-elem elem (cdr list-var) N test))))

(defun next-file (&optional reversep)
  (let* ((files
          (-remove #'f-dotname?
                   (-map #'f-filename (f-files (f-dirname (buffer-file-name)))))))
    (when reversep
      (setq files (nreverse files)))
    (nth-from-elem (f-filename (buffer-file-name)) files 1 #'string=)))

(defun open-next-file ()
  (interactive)
  (let ((fname (next-file)))
    (if fname
        (find-file fname)
      (message "Nothing to see anymore."))))

(defun open-previous-file ()
  (interactive)
  (let ((fname (next-file t)))
    (if fname
        (find-file fname)
      (message "Nothing to see anymore."))))

(defun delete-file-if-no-contents ()
  (let* ((trash delete-by-moving-to-trash)
         (verb (if trash "Trash" "Delete")))
    (when (and (= (point-min) (point-max))
               (buffer-file-name (current-buffer))
               (y-or-n-p (concat verb " file and kill buffer?")))
    (dired-delete-file (buffer-file-name (current-buffer)) :trash trash)
    (kill-buffer (current-buffer)))))

(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))

;; #dired
(defun dired-sort-size ()
  (interactive)
  (dired-sort-other (concat dired-listing-switches "S")))

(defun dired-sort-extension ()
  (interactive)
  (dired-sort-other (concat dired-listing-switches "X")))

(defun dired-sort-ctime ()
  (interactive)
  (dired-sort-other (concat dired-listing-switches "ct")))

(defun dired-sort-utime ()
  (interactive)
  (dired-sort-other (concat dired-listing-switches "ut")))

(defun dired-sort-time ()
  (interactive)
  (dired-sort-other (concat dired-listing-switches "t")))

(defun dired-sort-name ()
  (interactive)
  (dired-sort-other dired-listing-switches ))

(defun evil-swap-key (map key1 key2)
  "Swap KEY1 and KEY2 in MAP."
  (let ((def1 (lookup-key map key1))
        (def2 (lookup-key map key2)))
    (define-key map key1 def2)
    (define-key map key2 def1)))

(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (--first (find-font (font-spec :name it)) fonts))

;; #mouse
(defun scroll-down-with-lines ()
  " Able to Mouse hoil scroll. "
  (interactive)
  (scroll-down 4))

(defun scroll-up-with-lines ()
  " It Up. "
  (interactive)
  (scroll-up 4))

(provide 'user-defines)
