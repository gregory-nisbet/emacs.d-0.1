(require 'cl-lib)
(cl-pushnew "~/.emacs.d/lisp" load-path)
(cl-pushnew "~/.emacs.d/modules/evil-mode" load-path)
(cl-pushnew "~/.emacs.d/modules/evil-leader" load-path)
(cl-pushnew "~/.emacs.d/modules/nlinum" load-path)
(cl-pushnew "~/.emacs.d/modules/undo-tree" load-path)
(cl-pushnew "~/.emacs.d/modules/tuareg" load-path)
(cl-pushnew "~/.emacs.d/modules/php-mode" load-path)
(cl-pushnew "~/.emacs.d/modules/go-mode" load-path)
(cl-pushnew "~/.emacs.d/changed-maps" load-path)

;; I don't really use the universal argument for anything, so this keybinding can stay.
(setq evil-want-C-u-scroll t) ; have to set this before loading evil

(require 'my-evil) ; evil-related helper functions
(require 'evil-leader) ; manage bindings beginning with "<SPC>"
(require 'nlinum) ; better line numbering
(require 'evil)
(require 'tuareg) ; ocaml
(require 'php-mode) ; php
(require 'org)
(require 'go-mode)

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


;; use ssh instead of sftp for tramp mode by default.
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

;; in all evil-modes hjkl uo are used to navigate different buffers visually.
(my-evil/modes "i" "C-u" #'kill-whole-line) ; pleasant unicism
(my-evil/modes "vionmre" "M-j" #'evil-window-down)
(my-evil/modes "vionmre" "M-h" #'evil-window-left)
(my-evil/modes "vionmre" "M-k" #'evil-window-up)
(my-evil/modes "vionmre" "M-l" #'evil-window-right)
(my-evil/modes "vionmre" "M-u" #'previous-buffer)
(my-evil/modes "vionmre" "M-o" #'next-buffer)

;; normal-mode arrow keys for buffer navigation
(my-evil/modes "n" "<up>" #'other-window)
(my-evil/modes "n" "<down>" #'(lambda () (interactive) (other-window -1)))
(my-evil/modes "n" "<left>" #'previous-buffer)
(my-evil/modes "n" "<right>" #'next-buffer)

;; evil-mode search forward and backward from isearch
(my-evil/modes "n" "s" #'evil-search-forward)
(my-evil/modes "n" "r" #'evil-search-backward)
;; evil-mode behave more like vim setup
;; j and k go to previous and next visual line
(my-evil/modes "n" "j" #'evil-next-visual-line)
(my-evil/modes "n" "k" #'evil-previous-visual-line)

;; I use alt for navigation between different buffers
;; so I am moving the lower-frequency commands I shadowed to C-c
(global-set-key (kbd "C-c h") #'mark-paragraph)
(global-set-key (kbd "C-c j") #'indent-new-comment-line)
(global-set-key (kbd "C-c k") #'kill-sentence)
(global-set-key (kbd "C-c l") #'downcase-word)
(global-set-key (kbd "C-c o") facemenu-keymap)
(global-set-key (kbd "C-c u") #'upcase-word)
;; I frequently need to recenter so I am using f5 for that.
(global-set-key (kbd "<f5>") #'recenter-top-bottom)
(global-set-key (kbd "M-SPC") #'hippie-expand)
 
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
  "SPC" #'evil-forward-WORD-begin
  (kbd "w") #'save-buffer
  (kbd "a") #'evil-beginning-of-visual-line
  (kbd "e") #'evil-end-of-visual-line
  (kbd "j") #'join-line
  (kbd "o") #'open-line
  (kbd ",") #'evil-repeat-find-char-reverse
  (kbd "u") #'universal-argument
  (kbd "g") #'recentf-open-files)
;; workaround for bug in evil-leader
  (kbd "s") #'evil-substitute
  (kbd "r") #'evil-replace) 

;; load and patch comint 

;; we need to patch the universal argument map
(define-key universal-argument-map (kbd "C-u") nil)
(define-key universal-argument-map (kbd "SPC u") 'universal-argument-more)

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
;; recentf is the initial buffer way more useful than scratch
;; I guess one way to do this is just to call the function
(setq initial-buffer (recentf-open-files))
;; highlighting for current line (seems to work well with a light theme like leuven
(load-theme 'leuven)
(require 'highlight-current-line)
(highlight-current-line-on t)
