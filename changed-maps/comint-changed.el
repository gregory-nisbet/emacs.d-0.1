;; the first of many modes to be patched to use just the leader key.

;; 
;; `t' Minor Mode Bindings Starting With C-c:
;; key             binding
;; ---             -------

;; C-c C-a         comint-bol-or-process-mark
;; C-c C-b         shell-backward-command
;; C-c C-c         comint-interrupt-subjob
;; C-c C-d         comint-send-eof
;; C-c C-e         comint-show-maximum-output
;; C-c C-f         shell-forward-command
;; C-c C-l         comint-dynamic-list-input-ring
;; C-c RET         comint-copy-old-input
;; C-c C-n         comint-next-prompt
;; C-c C-o         comint-delete-output
;; C-c C-p         comint-previous-prompt
;; C-c C-r         comint-show-output
;; C-c C-s         comint-write-output
;; C-c C-u         comint-kill-input
;; C-c C-w         backward-kill-word
;; C-c C-x         comint-get-next-from-history
;; C-c C-z         comint-stop-subjob
;; C-c ESC         Prefix Command
;; C-c C-\         comint-quit-subjob
;; C-c SPC         comint-accumulate
;; C-c .           comint-insert-previous-argument

;; C-c M-r         comint-previous-matching-input-from-input
;; C-c M-s         comint-next-matching-input-from-input

;; 
;; Major Mode Bindings Starting With C-c:
;; key             binding
;; ---             -------

;; 
;; Global Bindings Starting With C-c:
;; key             binding
;; ---             -------

;; [back]


(setf comint-changed//shortcut-map
      '(
        ("a"         comint-bol-or-process-mark)
        ("b"         shell-backward-command)
        ("c"         comint-interrupt-subjob)
        ("d"         comint-send-eof)
        ("e"         comint-show-maximum-output)
        ("f"         shell-forward-command)
        ("l"         comint-dynamic-list-input-ring)
        ("RET"         comint-copy-old-input)
        ("n"         comint-next-prompt)
        ("o"         comint-delete-output)
        ("N"         comint-previous-prompt)
        ("r"         comint-show-output)
        ("s"         comint-write-output)
        ("u"         comint-kill-input)
        ("w"         backward-kill-word)
        ("x"         comint-get-next-from-history)
        ("z"         comint-stop-subjob)
        ("\\"         comint-quit-subjob)
        ("SPC"         comint-accumulate)
        ("."           comint-insert-previous-argument)

        ("R"         comint-previous-matching-input-from-input)
        ("S"         comint-next-matching-input-from-input))) 

(defun comint-changed//join-kbd (list)
  "join keys space separated list"
  (mapconcat #'identity list " "))

(setf comint-changed//evil-map
      (mapcar
       (lambda (args)
         "populate evil map with full key to func alist"
         (let ((key (car args))
               (func (cadr args)))
           (list (kbd (comint-changed//join-kbd (list "SPC" "f" key)))
                 func)))
       comint-changed//shortcut-map))

(defun comint-changed/populate-evil-map ()
  "add alist keys to evil map"
  (mapcar (lambda (args) 
            "populate keymap "
            (let ((key (car args))
                  (func (cadr args)))
              (evil-define-key 'normal comint-mode-map key func)))
          comint-changed//evil-map))

(provide 'comint-changed)
