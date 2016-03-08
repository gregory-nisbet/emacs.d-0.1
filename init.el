(require 'cl-lib)
(cl-pushnew "~/.emacs.d/lisp" load-path)
(cl-pushnew "~/.emacs.d/modules/evil-mode" load-path)
(cl-pushnew "~/.emacs.d/modules/evil-leader" load-path)
(cl-pushnew "~/.emacs.d/modules/nlinum" load-path)
(cl-pushnew "~/.emacs.d/modules/undo-tree" load-path)
(cl-pushnew "~/.emacs.d/modules/tuareg" load-path)
(cl-pushnew "~/.emacs.d/modules/php-mode" load-path)
(cl-pushnew "~/.emacs.d/modules/go-mode" load-path)
(cl-pushnew "~/.emacs.d/modules/scala-mode2" load-path)
(cl-pushnew "~/.emacs.d/modules/haskell-mode" load-path)
(cl-pushnew "~/.emacs.d/modules/clojure-mode" load-path)
(cl-pushnew "~/.emacs.d/modules/magit/lisp" load-path)
(cl-pushnew "~/.emacs.d/modules/dash" load-path)
(cl-pushnew "~/.emacs.d/modules/git-modes" load-path)
(cl-pushnew "~/.emacs.d/modules/with-editor" load-path)
(cl-pushnew "~/.emacs.d/modules/proof-general/coq" load-path)
(cl-pushnew "~/.emacs.d/modules/proof-general/generic" load-path)
(cl-pushnew "~/.emacs.d/changed-maps" load-path)


;; new-shell defun (stolen from tikhon jelvis' emacs config

;; todo make space exit the minibuffer prompt for certain things

                                        ; SHELL BUFFERS
;; I want an easy command for opening new shells:
(defun new-shell (name)
  "Opens a new shell buffer with the given name in
asterisks (*name*) in the current directory with and changes the
prompt to name>."
  
  (interactive "sName: ")
  ;; decorate name with asterisks for some reason
  (pop-to-buffer (concat "*" name "*"))
  (unless (eq major-mode 'shell-mode)
    (shell (current-buffer))
    (sleep-for 0 200)
    (delete-region (point-min) (point-max))
    (comint-simple-send (get-buffer-process (current-buffer))
                        ;; squiggly
                        (concat "export PS1=\"\033[33m" name "\033[0m:\033[35m\\W\033[0m>\""))))
(global-set-key (kbd "C-c s") 'new-shell)

;; doesn't work yet
(defun connect-remote-ssh (username hostname)
  "connect to a remote machine with name (name)"
  (interactive "sUser: \nsHost: ")
  (let
      ((file-loc (format "/:ssh:%s@%s:/" username hostname)))
    (find-file file-loc)))
(global-set-key (kbd "C-c SPC") 'connect-remote-ssh)


;; todo, also include a fairly minimal set of global key rebindings to make emacs more ergonomic.
;; so convenient keys for navigation and forward/backward word

;; todo recentf complains when a vm is down and it can't access the machine.
;; this is really obnoxious for the splash page.

;; todo damian conway's vimrc recommends visual block
;; the normal emacs rectangular highlight is better than visual block



;; todo emacs mode for dired has an issue where C-u in emacs state does not preserve
;; the C-u command for doubling up on universal arguments.
;; I think it would make more sense to make the C-u behavior depend on whether you are in an
;; evil-like state or a vim-like state

;; todo, make a more evil-like dired with more advanced fuzzy searching capabilities

;; still getting weird tramp mode errors every time I start up. what the heck is going on?

;; it makes a lot of sense to 
;; I don't really use the universal argument for anything, so this keybinding can stay.
(setq evil-want-C-u-scroll t) ; have to set this before loading evil

;;  prevent coq from compiling right off the bat
;; (setq coq-compile-before-require nil)
;; I have no idea why you might need this
;; (setq coq-compile-parallel-in-background nil)

(require 'my-evil) ; evil-related helper functions
(require 'evil-leader) ; manage bindings beginning with "<SPC>"
(require 'nlinum) ; better line numbering
(require 'evil)
(require 'tuareg) ; ocaml
(require 'php-mode) ; php
(require 'org)
(require 'go-mode) ; for golang
(require 'scala-mode2) ; scala
(require 'haskell-mode) ; haskell
(require 'magit) ; excellent git interface (or so I've heard)
(require 'highlight-current-line)

;; according to my sources, this should be enough
(require 'proof-site "~/.emacs.d/modules/proof-general/generic/proof-site")

;; configuration that doesn't need to come first.

(setq inhibit-startup-message t)
;; custom linum format for greater legibility
;; note that linum still sucks when you increase the text size
(setq nlinum-format " %d  ")
(setq visual-bell t)
(setq ring-bell-function #'ignore)   
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq disabled-command-function nil)
(setq recentf-max-menu-items 25)
(setq enable-recursive-minibuffers)
(setq evil-regexp-search t) ; search uses regular expressions
;; http://stackoverflow.com/questions/3281581/how-to-word-wrap-in-emacs
(setq-default word-wrap t) ; enables word wrap without altering the behavior of C-k
;; use allman style for C
(cl-pushnew '(c-mode . "bsd") c-default-style)

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
;; just like nnoremap j gj and nnoremap k gk
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
;; sometimes M-SPC is claimed by the window manager, so
;; this is just a cheap hack so that hippie-expand is still usable in
;; situations like lxde where meta-space is similar to menu key
(global-set-key (kbd "M-/") #'hippie-expand)

;; super a s d f g are reserved for emacs, other super are reserved for the window manager
;; these are chosen so as not to conflict with the default xmonad bindings
(my-evil/modes "vionmre" "s-f" #'find-file)
(global-set-key (kbd "s-f") #'find-file)
(my-evil/modes "vionmre" "s-s" #'save-buffer)
(global-set-key (kbd "s-s") #'save-buffer)

;; evil configuration
(add-to-list 'evil-emacs-state-modes 'nav-mode)

;; Evil leader section

(global-evil-leader-mode +1)
(evil-mode +1)
(global-evil-leader-mode +1)
;; set-leader is finicky and doesn't reject keys it doesn't understand
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "SPC" #'(lambda () (interactive) (evil-next-visual-line 16))
  (kbd "w") #'save-buffer
  (kbd "a") #'evil-beginning-of-visual-line
  (kbd "e") #'evil-end-of-visual-line
  (kbd "j") #'join-line
  (kbd "o") #'open-line
  (kbd "u") #'universal-argument
  (kbd "g") #'recentf-open-files
  (kbd "s") #'evil-substitute
  (kbd "r") #'evil-replace) 

;; we need to patch the universal argument map
(define-key universal-argument-map (kbd "C-u") nil)
(define-key universal-argument-map (kbd "SPC u") 'universal-argument-more)

(require 'comint-changed)
(comint-changed/populate-evil-map)
;; replace the comint C-d with page down
(define-key comint-mode-map (kbd "C-d") nil) 

;; other miscellaneous changes
(show-paren-mode +1)

;; alias
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer)
;; cperl mode is better than perl
(defalias 'perl-mode 'cperl-mode)
(load-theme 'leuven)
(highlight-current-line-on t)
;; I need to fix this section below, but for the moment I will just use the <spc>-g binding
;; recentf is the initial buffer way more useful than scratch
;; I guess one way to do this is just to call the function
;; blunt instrument. silence the no recent files errors
;; (ignore-errors (setq initial-buffer (recentf-open-files)))
;; highlighting for current line (seems to work well with a light theme like leuven

;; recentf can fail to initialize in the case of inaccessible hosts over tramp mode.
;; seems weird
;; ignore errors here only
(unwind-protect
    (recentf-mode +1)
  ;; open recent files in splash page
  (recentf-open-files))
