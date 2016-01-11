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

(provide 'my-evil)
