(require 'cl-lib)
(cl-pushnew "~/.emacs.d/lisp" load-path)
(cl-pushnew "~/.emacs.d/modules/evil-mode" load-path)
(cl-pushnew "~/.emacs.d/modules/evil-leader" load-path)
(cl-pushnew "~/.emacs.d/modules/nlinum" load-path)
(cl-pushnew "~/.emacs.d/modules/undo-tree" load-path)
(cl-pushnew "~/.emacs.d/modules/tuareg" load-path)
(cl-pushnew "~/.emacs.d/modules/php-mode" load-path)
(cl-pushnew "~/.emacs.d/changed-maps" load-path)

(setq evil-want-C-u-scroll t) ; have to set this before loading evil

(require 'my-evil) ; evil-related helper functions
(require 'evil-leader) ; manage bindings beginning with "<SPC>"
(require 'nlinum) ; better line numbering
(require 'evil)
(require 'tuareg) ; ocaml
(require 'php-mode) ; php
(require 'org)

;; configuration that doesn't need to come first.

(setq inhibit-startup-message t)
(setq nlinum-format "%d  ")
(setq visual-bell t)
(setq ring-bell-function #'ignore)   
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq disabled-command-function nil)
(setq recentf-max-menu-items 25)
(setq enable-recursive-minibuffers)
(setq evil-regexp-search t) ; search uses regular expressions

(setq tramp-default-method "ssh")
;; make the latex fragments bigger
;; http://tex.stackexchange.com/questions/78501/change-size-of-the-inline-image-for-latex-fragment-in-emacs-org-mode
(plist-put org-format-latex-options :scale 1.5)
;; configure evil-mode


;; current policy is that none of the original emacs keybindings
;; (with the possible exception of C-x) should be visible from
;; within evil-mode. keybindings beginning with the meta key
;; are useful shortcuts, but should be accessible in other ways

;; convenient vim binding, many emacs packages have the keybindings hardcoded in
;; so this will prevent them from being expressed.
(my-evil/modes "vionmr" "C-l" #'evil-normal-state) ; extremely convenient vim binding
(my-evil/modes "vionmr" "C-k" #'evil-normal-state) ; I have never once wanted to use a digraph.
;; plus aren't there more convenient ways to enter different scripts?


(my-evil/modes "i" "C-u" #'kill-whole-line) ; pleasant unicism
(my-evil/modes "vionmre" "M-j" #'evil-window-down)
(my-evil/modes "vionmre" "M-h" #'evil-window-left)
(my-evil/modes "vionmre" "M-k" #'evil-window-up)
(my-evil/modes "vionmre" "M-l" #'evil-window-right)
(my-evil/modes "vionmre" "M-u" #'previous-buffer)
(my-evil/modes "vionmre" "M-o" #'next-buffer)

(global-set-key (kbd "C-c h") #'mark-paragraph)
(global-set-key (kbd "C-c j") #'indent-new-comment-line)
(global-set-key (kbd "C-c k") #'kill-sentence)
(global-set-key (kbd "C-c l") #'downcase-word)
(global-set-key (kbd "C-c o") facemenu-keymap)
(global-set-key (kbd "C-c u") #'upcase-word)

;; super a s d f g are reserved for emacs, other super are reserved for the window manager
(my-evil/modes "vionm" "s-f" #'find-file)

;; evil configuration
(add-to-list 'evil-emacs-state-modes 'nav-mode)

;; Evil leader section

(global-evil-leader-mode +1)
(evil-mode +1)
(global-evil-leader-mode +1)
;; set-leader is finicky and doesn't reject keys it doesn't understand
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  (kbd "w") #'save-buffer
  (kbd "a") #'evil-beginning-of-visual-line
  (kbd "e") #'evil-end-of-visual-line
  (kbd "j") #'join-line
  (kbd "o") #'open-line)

;; load and patch comint 

(require 'comint-changed)
(comint-changed/populate-evil-map)
;; replace the comint C-d with page down
(define-key comint-mode-map (kbd "C-d") nil) 

;; other miscellaneous changes
(recentf-mode +1)
(show-paren-mode +1)

;; alias
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer)
