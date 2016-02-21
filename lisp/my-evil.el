;; helper functions for adding keys.
(defun my-evil/normal (key function)
  "set normal mode key"
  (cl-assert (stringp key) t)
  (cl-assert (functionp function) t)
  (let ((kbd (kbd key)))
    (define-key evil-normal-state-map kbd function)))

(defun my-evil/insert (key function)
  "set insert mode"
  (cl-assert (stringp key) t)
  (cl-assert (functionp function) t)
  (let ((kbd (kbd key)))
    (define-key evil-insert-state-map kbd function)))

(defun my-evil/operator (key function)
  "set key in operator mode"
  (cl-assert (stringp key) t)
  (cl-assert (functionp function) t)
  (let ((kbd (kbd key)))
    (define-key evil-operator-state-map kbd function)))

(defun my-evil/visual (key function)
  "set key in visual mode"
  (cl-assert (stringp key) t)
  (cl-assert (functionp function) t)
  (let ((kbd (kbd key)))
    (define-key evil-visual-state-map kbd function)))

(defun my-evil/motion (key function)
  "set key in motion state"
  (cl-assert (stringp key) t)
  (cl-assert (functionp function) t)
  (let ((kbd (kbd key)))
    (define-key evil-motion-state-map kbd function)))

(defun my-evil/replace (key function)
  "set key in replace state"
  (cl-assert (stringp key) t)
  (cl-assert (functionp function) t)
  (let ((kbd (kbd key)))
    (define-key evil-replace-state-map kbd function)))

(defun my-evil/replace (key funcion)
  "set key in emacs state"
  (cl-assert (stringp key) t)
  (cl-assert (functionp function) t)
  (let ((kbd (kbd key)))
    (define-key evil-replace-state-map kbd function)))

(defun my-evil/emacs (key funcion)
  "set key in emacs state"
  (cl-assert (stringp key) t)
  (cl-assert (functionp function) t)
  (let ((kbd (kbd key)))
    (define-key evil-emacs-state-map kbd function)))

(defun my-evil//normalize-mode-sequence (modes)
  "internal: normalize \"ovi\" to '(o v i) for use in my-evil/modes"
  (cl-assert (or (stringp modes) (consp modes)))
  (cond
   ((stringp modes)
    (mapcar #'char-to-string (string-to-list modes)))
   ((t modes))))

(defun my-evil/modes (modes key function)
  "set key in modes"
  (cl-assert (or (stringp modes) (consp modes)) t)
  (cl-assert (functionp function) t)
  (let*
      ((modes (my-evil//normalize-mode-sequence modes))
       (wanted-function 
    (mapcar (lambda (k)
          "associate k with an abbreviation for a mode or die"
          (cond
           ((string= 'v k) #'my-evil/visual)
           ((string= 'i k) #'my-evil/insert)
           ((string= 'o k) #'my-evil/operator)
           ((string= 'n k) #'my-evil/normal)
           ((string= 'm k) #'my-evil/motion)
           ((string= 'r k) #'my-evil/replace)
           ((string= 'e k) #'my-evil/emacs)
           (t (cl-assert nil nil "function not one of the accepted abbreviations for evil mode"))))
        modes)))
    (mapcar (lambda (f)
              "apply each wanted function to key and function"
              (apply f (list key function))) wanted-function)))

;; (evil-define-motion evil-forward-word-begin-with-limit (count bigword limit)
;;   "Move the cursor to the beginning of the COUNT-th next word.
;; If BIGWORD is non-nil, move by WORDS.

;; If this command is called in operator-pending state it behaves
;; differently. If point reaches the beginning of a word on a new
;; line point is moved back to the end of the previous line.

;; If called after a change operator, i.e. cw or cW,
;; `evil-want-change-word-to-end' is non-nil and point is on a word,
;; then both behave like ce or cE.

;; If point is at the end of the buffer and cannot be moved signal
;; 'end-of-buffer is raised.
;; "
;;   :type exclusive
;;   (let ((thing (if bigword 'evil-WORD 'evil-word))
;;         (orig (point))
;;         (count (or count 1)))
;;     (evil-signal-at-bob-or-eob count)
;;     (cond
;;      ;; default motion, beginning of next word
;;      ((not (evil-operator-state-p))
;;       (evil-forward-beginning thing count))
;;      ;; the evil-change operator, maybe behave like ce or cE
;;      ((and evil-want-change-word-to-end
;;            (eq evil-this-operator #'evil-change)
;;            (< orig (or (cdr-safe (bounds-of-thing-at-point thing)) orig)))
;;       ;; forward-thing moves point to the correct position because
;;       ;; this is an exclusive motion
;;       (forward-thing thing count))
;;      ;; operator state
;;      (t
;;       (prog1 (evil-forward-beginning thing count)
;;         ;; if we reached the beginning of a word on a new line in
;;         ;; Operator-Pending state, go back to the end of the previous
;;         ;; line
;;         (when (and (> (line-beginning-position) orig)
;;                    (looking-back "^[[:space:]]*" (line-beginning-position)))
;;           ;; move cursor back as long as the line contains only
;;           ;; whitespaces and is non-empty
;;           (evil-move-end-of-line 0)
;;           ;; skip non-empty lines containing only spaces
;;           (while (and (looking-back "^[[:space:]]+$" (line-beginning-position))
;;                       (not (<= (line-beginning-position) orig)))
;;             (evil-move-end-of-line 0))
;;           ;; but if the previous line is empty, delete this line
;;          (when (bolp) (forward-char)))))))) 

(provide 'my-evil)
