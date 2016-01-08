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
(my-evil/modes "vionmr" "M-j" #'evil-window-down)
(my-evil/modes "vionmr" "M-h" #'evil-window-left)
(my-evil/modes "vionmr" "M-k" #'evil-window-up)
(my-evil/modes "vionmr" "M-l" #'evil-window-right)
(my-evil/modes "vionmr" "M-u" #'previous-buffer)
(my-evil/modes "vionmr" "M-o" #'next-buffer)


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

;; other miscellaneous changes
(recentf-mode +1)
(show-paren-mode +1)
