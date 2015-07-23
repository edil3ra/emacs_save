(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-verbose t)
(require 'use-package)

(use-package helm
  :ensure t
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
	  helm-input-idle-delay 0.01  ; this actually updates things
	  helm-yas-display-key-on-candidate t
	  helm-quick-update t
	  helm-M-x-requires-pattern nil
	  helm-M-x-fuzzy-match t
	  helm-ff-skip-boring-files t
	  helm-buffers-fuzzy-matching t
	  helm-recentf-fuzzy-match t
	  helm-locate-fuzzy-match t
	  helm-split-window-in-side-p t
	  helm-scroll-amount 8
	  helm-autoresize-mode 1)
    (helm-mode))
  :config
  (progn
    (bind-key  "<tab>" #'helm-execute-persistent-action helm-map) ; rebind tab to do persistent action
    (bind-key  "C-i" #'helm-execute-persistent-action helm-map) ; make TAB works in terminal
    (bind-key  "C-z" #'helm-select-action helm-map) ; list actions using C-z
    (bind-key  "M-c" #'helm-previous-line helm-map)
    (bind-key  "M-t" #'helm-next-line helm-map)
    (bind-key  "M-C" #'helm-previous-page helm-map)
    (bind-key  "M-T" #'helm-next-page helm-map)
    (bind-key  "M-b" #'helm-beginning-of-buffer helm-map)
    (bind-key  "M-B" #'helm-end-of-buffer helm-map)
    (bind-key  "M-C" #'helm-previous-page helm-find-files-map)
    (bind-key  "M-B" #'helm-end-of-buffer helm-find-files-map)
    (bind-key  "C-f" #'helm-ff-run-find-sh-command helm-find-files-map)
    (bind-key  "C-S-f" #'helm-ff-run-locate helm-find-files-map)
    (bind-key  "C-d" #'helm-buffer-run-kill-persistent helm-buffer-map)
    (bind-key  "C-S-d" #'helm-buffer-run-kill-buffers helm-buffer-map)
    (bind-key  "M-SPC" #'helm-toggle-visible-mark helm-map)))


(use-package helm-swoop
  :ensure t :defer t)

(use-package helm-projectile
  :ensure t :defer t)

(use-package helm-css-scss
  :ensure t :defer t)


(use-package company
  :ensure t
  :init (progn
	  (add-hook 'prog-mode-hook 'company-mode)
	  (add-hook 'html-mode-hook 'company-mode)
	  (add-hook 'css-mode-hook 'company-mode)
	  (add-hook 'scss-mode-hook 'company-mode)
	  ;; (global-company-mode 1)
	  (setq company-tooltip-limit 20) ; bigger popup window
	  (setq company-idle-delay 0.5)   ; decrease delay before autocompletion popup shows
	  (setq company-echo-delay 0)     ; remove annoying blinking
	  (setq company-show-numbers t)   ; show numbers for easy selection
	  (company-quickhelp-mode 1)
	  '(company-backends
	    (quote
	     (company-ghc company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf
			  (company-dabbrev-code company-gtags company-etags company-keywords)
			  company-oddmuse company-files company-dabbrev company-jedi company-tern))))
  
  :config (progn
            (bind-key "<tab>" #'company-complete company-active-map)
            (bind-key "M-/" #'company-show-doc-buffer company-active-map)
						(bind-key "C-c C-d" #'company-show-doc-buffer company-active-map)
						(bind-key "C-c d" #'company-show-doc-buffer company-active-map)
            (bind-key "M-l" #'company-show-location company-active-map)))


(use-package company-quickhelp
  :ensure t)


(use-package projectile
 :ensure t :defer t
 :init(progn
   (setq projectile-completion-system 'default)
   (setq projectile-enable-caching t)
   (setq projectile-completion-system 'helm)
   (setq projectile-use-native-indexing t)
   (projectile-global-mode)))


(use-package cua
  :init(progn
    (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
    (transient-mark-mode 1) ;; No region when it is not highlighted
    (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
    (cua-mode t)))


(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init(progn
	 (yas-global-mode 1))
  :config(progn
	   (yas-global-mode 0)
	   (unbind-key "<tab>" yas-minor-mode-map)
	   (unbind-key "C-SPC" yas-minor-mode-map)
	   (bind-key "C-i" #'yas-expand yas-minor-mode-map)))


(use-package smartparens
  :ensure t :defer t
  :init (progn
	  (smartparens-global-mode t)
	  (show-smartparens-global-mode t)))


(use-package rainbow-delimiters
  :ensure t :defer t
  :init (progn
	  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
	  (add-hook 'scss-mode-hook #'rainbow-delimiters-mode)))


(use-package uniquify
  :init(progn
	   uniquify-buffer-name-style 'post-forward
	   uniquify-separator ":"))


(use-package framemove
  :ensure t)


(use-package auto-save-buffers-enhanced
  :ensure t
  :init(progn
	 (auto-save-buffers-enhanced t)))


(use-package drag-stuff
  :diminish drag-stuff-mode
  :ensure t :defer t
  :init(progn
	 (drag-stuff-global-mode t)))


(use-package expand-region
  :ensure t)


(use-package goto-chg
  :ensure t)


(use-package fullscreen-mode
  :ensure t
  :init(progn
	 (fullscreen-mode t)))


(use-package nyan-mode
  :ensure t :defer t
  :init(progn
	 (add-hook 'prog-mode-hook #'nyan-mode)))


(use-package multiple-cursors
  :ensure t :defer t)


(use-package dired
  :init(progn
	 (toggle-diredp-find-file-reuse-dir 1)
	 '(dired-clean-up-buffers-too nil)
	 '(dired-use-ls-dired t))
  :config(progn
	   (unbind-key  "M-c" dired-mode-map)))


(use-package dired+
  :ensure t)


(use-package ergoemacs-mode
:ensure t
:init(progn
 '(ergoemacs-keyboard-layout "dv")
 '(ergoemacs-mode nil)))


(use-package emmet-mode
  :ensure t :defer t
  :init(progn
	 (add-hook 'web-mode-hook 'emmet-mode)
	 (add-hook 'html-mode-hook 'emmet-mode)
	 (add-hook 'jinja2-mode-hook 'emmet-mode)
	 (add-hook 'css-mode-hook 'emmet-mode)
	 (setq emmet-indentation 2)
	 (setq emmet-preview-default nil)
	 (add-hook' emmet-mode-hook(lambda()
				     (bind-key "C-c C-w" #'emmet-wrap-with-markup emmet-mode-keymap)
				     (bind-key "C-c w" #'emmet-wrap-with-markup emmet-mode-keymap)))))


(use-package jinja2-mode
  :ensure t :defer t)


(use-package css-mode
  :ensure t
  :config(progn
	 (bind-key "C-p" #'helm-css-scss css-mode-map)))


(use-package scss-mode
 :ensure t
 :init(progn
	(setq scss-compile-at-save nil))
 :config(progn
	  (bind-key "C-p" #'helm-css-scss scss-mode-map)))


(use-package markdown-mode
  :ensure t :defer t
  :init(progn
	 '(markdown-xhtml-standalone-regexp "")))


(use-package json-mode
  :ensure t :defer t)


(use-package imenu
  :init(progn
	 (setq imenu-auto-rescan t)
	 (setq imenup-ignore-comments-flag nil)
	 (setq imenup-sort-ignores-case-flag nil)))


(use-package semantic
  :init(progn
	 (semantic-mode t)))


(use-package undo-tree
  :ensure t :defer t)


(use-package ido
  :defer t
  :init(progn
       (setq ibuffer-saved-filter-groups
	     (quote (("default"
		      ("dired" (mode . dired-mode))
		      ("html" (or
			       (mode . html-mode)
			       (mode . web-mode)
			       (mode . jinja2-mode)))
		      ("coffee" (mode . coffee-mode))
		      ("js" (mode . js3-mode))
		      ("coffee" (mode . coffee-mode))
		      ("haskell" (mode . haskell-mode))
		      ("python" (mode . python-mode))
		      ("css" (or
			      (mode . scss-mode)
			      (mode . css-mode)))
		      ("scratch" (or
				  (name . "^\\*scratch\\*$")
				  (name . "^\\*Messages\\*$")))
		      ("gnus" (or
			       (mode . message-mode)
			       (mode . bbdb-mode)
			       (mode . mail-mode)
			       (mode . gnus-group-mode)
			       (mode . gnus-summary-mode)
			       (mode . gnus-article-mode)
			       (name . "^\\.bbdb$")
			       (name . "^\\.newsrc-dribble")))))))
       (add-hook 'ibuffer-mode-hook
		 (lambda ()
		   (ibuffer-switch-to-saved-filter-groups "default")))
       (setq ido-ignore-buffers '("\\` " "^\*"))))


(use-package erc
  :defer t
  :init (progn
	  '(erc-autojoin-mode t)
	  '(erc-button-mode t)
	  '(erc-email-userid "lemarsupu@gmail.com")
	  '(erc-fill-mode t)
	  '(erc-irccontrols-mode t)
	  '(erc-list-mode t)
	  '(erc-match-mode t)
	  '(erc-menu-mode t)
	  '(erc-move-to-prompt-mode t)
	  '(erc-netsplit-mode t)
	  '(erc-networks-mode t)
	  '(erc-noncommands-mode t)
	  '(erc-pcomplete-mode t)
	  '(erc-readonly-mode t)
	  '(erc-ring-mode t)
	  '(erc-stamp-mode t)
	  '(erc-track-minor-mode t)
	  '(erc-track-mode t)))


(use-package org
  :defer t
  :init (progn
	  '(org-CUA-compatible nil)
	  '(org-babel-load-languages (quote ((python . t) (latex . t) (sh . t))))
	  '(org-pretty-entities t)
	  '(org-pretty-entities-include-sub-superscripts t)
	  '(org-replace-disputed-keys nil)))


;;PYTHON
(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :init (progn
	   (setq expand-region-preferred-python-mode (quote fgallina-python))
	   (setq python-shell-interpreter "ipython3")
	   (setq python-check-command nil)))
  

(use-package elpy
  :ensure t :defer t
  :init(progn
	 (elpy-enable)
	 (setq elpy-rpc-python-command "python3")
	 (setq elpy-rpc-backend "jedi")
	 (setq elpy-modules
	   (quote
	    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults)))))


(use-package jedi
  :ensure t :defer t)


(use-package pyvenv
  :ensure t :defer t)


;; (use-package js2-mode
;;   :ensure t
;;   :init(progn
;; 	 (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; 	 (add-hook 'js-mode-hook 'js2-minor-mode)))


;; JAVASCRIPT
(use-package js3-mode
  :ensure t
  :init(progn
	 '(js3-auto-indent-p t)         ; it's nice for commas to right themselves.
	 '(js3-enter-indents-newline t) ; don't need to push tab before typing
	 '(js3-indent-on-enter-key t)   ; fix indenting before moving on
	 ))

  
;; COFFEESCRIPT
(use-package coffee-mode
  :ensure t :defer t
  :init(progn
	 (set (make-local-variable 'tab-width) 2)
	 (setq coffee-tab-width 2)))


(use-package tern
  :ensure t :defer t
  :init(progn
	 (add-hook 'js-mode-hook (lambda () (tern-mode t)))
	 (add-hook 'coffee-mode-hook (lambda () (tern-mode t)))))


(use-package company-tern
  :ensure t :defer t
  :init (add-to-list 'company-backends 'company-tern))


;; LUA
(use-package lua-mode
  :ensure t :defer t
  :init(progn
	 '(lua-indent-level 2)
	 '(lua-prefix-key "C-c"))
  :config(progn
	   (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
	   (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))))


;; HASKELL
(use-package haskell-mode
  :ensure t
  :init(progn
	 (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
	 (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
	 (add-hook 'haskell-mode-hook 'turn-on-hi2)
	 (setq haskell-process-suggest-remove-import-lines t)
	 (setq haskell-process-auto-import-loaded-modules t)
	 (setq haskell-process-log t)
	 (setq haskell-process-type 'cabal-repl))
  :config(progn
	   (bind-key "C-c '" #'haskell-move-nested-left haskell-mode-map)
	   (bind-key "C-c ," #'haskell-move-nested-right haskell-mode-map)
	   (bind-key "C-`" #'haskell-interactive-bring haskell-mode-map)
	   (bind-key "C-c t" #'haskell-process-do-type haskell-mode-map)
	   (bind-key "C-c C-t" #'haskell-process-do-type haskell-mode-map)
	   (bind-key "C-c n" #'haskell-process-do-info haskell-mode-map)
	   (bind-key "C-c C-n" #'haskell-process-do-info haskell-mode-map)
	   (bind-key "C-c c" #'haskell-process-cabal haskell-mode-map)
	   (bind-key "C-c C-c" #'haskell-process-cabal-build haskell-mode-map)
	   (bind-key "C-c d" #'haskell-hoogle haskell-mode-map)
	   (bind-key "C-c C-d" #'haskell-hoogle haskell-mode-map)
	   (bind-key "C-c l" #'haskell-process-load-or-reload haskell-mode-map)
	   (bind-key "C-c C-l" #'haskell-process-load-or-reload haskell-mode-map)
	   (bind-key "C-c r" #'haskell-debug haskell-mode-map)
	   (bind-key "C-c C-r" #'haskell-debug haskell-mode-map)
	   (bind-key "C-c C-k" #'haskell-interactive-mode-clear haskell-mode-map)
	   (bind-key [f8] #'haskell-navigate-imports haskell-mode-map)))
	   
	   
;; (use-package company-ghc
;;   :ensure t
;;   :init (progn
;; 	  (add-to-list 'company-backends 'company-ghc)))


(use-package ghc
  :ensure t
  :init (progn
	  (autoload 'ghc-init "ghc" nil t)
	  (autoload 'ghc-debug "ghc" nil t)
	  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
	  ))

;; STUFF
;; remove window decoration
(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))


;; bookmark startup
(setq inhibit-splash-screen t)
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")



;; replace yes to y 
(fset 'yes-or-no-p 'y-or-n-p)
;; backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))


;; save on lost focus to change when i switch window
(when
    (and (featurep 'x) window-system)
  (defvar on-blur--saved-window-id 0 "Last known focused window.")
  (defvar on-blur--timer nil "Timer refreshing known focused window.")
  (defun on-blur--refresh ()
    "Runs on-blur-hook if emacs has lost focus."
    (let* ((active-window (x-window-property
                           "_NET_ACTIVE_WINDOW" nil "WINDOW" 0 nil t))
           (active-window-id (if (numberp active-window)
                                 active-window
                               (string-to-number
                                (format "%x00%x"
                                        (car active-window)
                                        (cdr active-window)) 16)))
           (emacs-window-id (string-to-number
                             (frame-parameter nil 'outer-window-id))))
      (when (and
             (= emacs-window-id on-blur--saved-window-id)
             (not (= active-window-id on-blur--saved-window-id)))
        (run-hooks 'on-blur-hook))
      (setq on-blur--saved-window-id active-window-id)
      (run-with-timer 1 nil 'on-blur--refresh)))
  (add-hook 'on-blur-hook #'(lambda () (save-some-buffers t)))
  (on-blur--refresh))


;; run current file
(defun xah-run-current-file ()
  (interactive)
  (let* (
         (suffixMap
          `(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python3")
            ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
            ("js" . "node")             ; node.js
            ("sh" . "bash")
            ("lua" . "lua")
            )
          )
         (fName (buffer-file-name))
         (fSuffix (file-name-extension fName))
         (progName (cdr (assoc fSuffix suffixMap)))
         (cmdStr (concat progName " \""   fName "\""))
         )
    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified. Do you want to save first?")
	(save-buffer)))
    (if (string-equal fSuffix "el") ; special case for emacs lisp
        (load (file-name-sans-extension fName))
      (if progName
          (progn
            (message "Running…")
            (shell-command cmdStr "*xah-run-current-file output*" )
            )
        (message "No recognized program file suffix for this file.")
        ))))



;; shell buffer
(defun my-filter (condp lst)
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))
(defun shell-dwim (&optional create)
  "Start or switch to an inferior shell process, in a smart way.  If 
  buffer with a running shell process exists, simply switch to that 
  If a shell buffer exists, but the shell process is not running, restart 
  shell.  If already in an active shell buffer, switch to the next one, 
  any.  With prefix argument CREATE always start a new shell."
  (interactive "P")
  (let ((next-shell-buffer) (buffer)
	(shell-buf-list (identity ;;used to be reverse                                                                         
			 (sort
			  (my-filter (lambda (x) (string-match "^\\*shell\\*" (buffer-name x))) (buffer-list))
			  #'(lambda (a b) (string< (buffer-name a) (buffer-name b)))))))
    (setq next-shell-buffer
	  (if (string-match "^\\*shell\\*" (buffer-name buffer))
	      (get-buffer (cadr (member (buffer-name) (mapcar (function buffer-name) (append shell-buf-list shell-buf-list)))))
	    nil))
    (setq buffer
	  (if create
	      (generate-new-buffer-name "*shell*")
	    next-shell-buffer))
    (shell buffer)))


(defun shell_buffer ()
  (interactive)
  (switch-to-buffer "*Shell Command Output*"))


(defun split-3-windows-horizontally-evenly ()
  (interactive)
  (command-execute 'split-window-horizontally)
  (command-execute 'split-window-horizontally)
  (command-execute 'balance-windows))


(defun org-export-latex-format-toc-org-article (depth)
  (when depth
    (format "\\setcounter{secnumdepth}{%s}\n\\tableofcontents\n"
            depth)))
(setq org-export-latex-format-toc-function 'org-export-latex-format-toc-org-article)


(defun smart-ret()
  (interactive)
  (end-of-line)
  (newline-and-indent))


;; KEYBOARD
;; MARK COMMAND, COMPLETE, YAS, TAB
(bind-key "M-SPC" 'set-mark-command)
(bind-key "C-SPC" 'company-complete)
(bind-key  "C-." 'keyboard-espace-quit)
(bind-key "<escape>" 'keyboard-espace-quit)
(bind-key "C-a" 'mark-whole-buffer)
(bind-key "M-m" 'emmet-expand-line)
(bind-key "C-x j" 'dired-jump)
;; (define-key key-translation-map (kbd "<f8>") (kbd "<menu>"))


;; MOVE KEY
(bind-key "M-c" 'previous-line)
(bind-key "M-t" 'next-line)
(bind-key* "M-h" 'backward-char)
(bind-key* "M-n" 'forward-char)
(bind-key* "M-g" 'backward-word)
(bind-key* "M-r" 'forward-word)
(bind-key "M-C" 'scroll-down-command)
(bind-key "M-T" 'scroll-up-command)
(bind-key* "M-H" 'ergoemacs-backward-close-bracket)
(bind-key* "M-N" 'ergoemacs-forward-close-bracket)
(bind-key* "M-G" 'ergoemacs-backward-block)
(bind-key* "M-R" 'ergoemacs-forward-block)
(bind-key* "M-d" 'ergoemacs-beginning-of-line-or-what)
(bind-key* "M-D" 'ergoemacs-end-of-line-or-what)
(bind-key "M-b" 'ergoemacs-beginning-or-end-of-buffer)
(bind-key "M-B" 'ergoemacs-end-or-beginning-of-buffer)



;; DELETE KEY
(bind-key* "M-e" 'backward-delete-char-untabify)
(bind-key* "M-u" 'delete-char)
(bind-key* "M-." 'backward-kill-word)
(bind-key* "M-p" 'kill-word)
(bind-key* "M-i" 'kill-line)
(bind-key* "M-I" 'ergoemacs-kill-line-backward)


;; COPY, CUT, PASTE, REDO, UNDO
(bind-key* "M-q" 'ergoemacs-cut-line-or-region)
(bind-key* "M-j" 'ergoemacs-copy-line-or-region)
(bind-key* "M-k" 'ergoemacs-paste)
(bind-key* "M-Q" 'ergoemacs-cut-all)
(bind-key* "M-J" 'ergoemacs-copy-all)
(bind-key* "M-K" 'ergoemacs-paste-cycle)
(bind-key* "M-;" 'undo-tree-undo)
(bind-key* "M-:" 'undo-tree-redo)
(bind-key "C-z" 'undo-tree-undo)
(bind-key* "C-S-z" 'undo-tree-redo)


;; POP, SAVE, GOTO, INFO, SCALE, CAMEL, RECENTER
(bind-key* "M-f" 'goto-last-change)
(bind-key* "M-F" 'goto-last-change-reverse)
(bind-key* "C-s" 'save-buffer)
(bind-key* "C-S-s" 'ido-write-file)
(bind-key* "C-l" 'goto-line)
(bind-key* "C-/" 'helm-info-at-point)
(bind-key* "C-=" 'text-scale-increase)
(bind-key* "C--" 'text-scale-decrease)
(bind-key* "M-z" 'ergoemacs-toggle-letter-case)
(bind-key* "C-S-z" 'ergoemacs-toggle-camel-case)
(bind-key* "M-9" 'recenter-top-bottom)


;; NEW BUFFER, FRAME CLOSE BUFFER, COMMENT
(bind-key "C-n" 'ergoemacs-new-empty-buffer)
(bind-key* "C-b"  'make-frame-command)
(bind-key* "C-w" 'ergoemacs-close-current-buffer)
(bind-key* "C-S-w" 'delete-frame)
(bind-key* "M--" 'comment-dwim)


;; COMMAND, SHELL, RUN, EMMET
(bind-key* "M-a" 'helm-M-x)
(bind-key* "M-A" 'shell-command)
(bind-key* "M-1" 'shell-dwim)
(bind-key* "<f1>" 'shell_buffer)
(bind-key* "<f5>" 'xah-run-current-file)
(bind-key* "<f6>" 'helm-recentf)
(bind-key* "<f7>" 'helm-bookmarks)
(bind-key* "<f12>" 'fullscreen-mode-fullscreen-toggle) 


(bind-key* "C-o" 'helm-find-files)
(bind-key* "M-o" 'helm-projectile-find-file-dwim)
(bind-key "C-p" 'helm-semantic-or-imenu)
(bind-key "C-y" 'helm-show-kill-ring)
(bind-key "C-f" 'helm-projectile-switch-to-buffer)
(bind-key "C-S-f" 'helm-locate)
(bind-key "C-h a" 'helm-apropos)
(global-set-key (kbd "C-e") 'helm-buffers-list)


;; HELM SWOOP
(bind-key* "C-r" 'helm-swoop)
(bind-key* "C-S-r" 'helm-swoop-back-to-last-point)
(bind-key* "M-7" 'helm-multi-swoop)
(bind-key* "M-8" 'helm-multi-swoop-all)


;; SELECTION RETURN 
(bind-key* "M-l" 'ergoemacs-select-current-line)
(bind-key* "M-L" 'ergoemacs-select-current-block)
(bind-key* "M-S" 'er/mark-inside-pairs)
(bind-key* "M-s" 'er/expand-region)
(bind-key* "M-RET" 'smart-ret)


;; BUFFER SWITCHING ENANCEMENT
(bind-key* "M-'" 'ergoemacs-previous-user-buffer)
(bind-key* "M-," 'ergoemacs-next-user-buffer)
(bind-key* "M-<" 'register-to-point)
(bind-key* "M-\"" 'point-to-register)
(bind-key* "C-S-e" 'ibuffer)


;; MULTIPLE CURSORS
(bind-key "C-d" 'mc/mark-next-like-this)
(bind-key "C-S-d" 'mc/mark-all-like-this)
(unbind-key "M-<down-mouse-1>")
(bind-key* "M-<mouse-1>" 'mc/add-cursor-on-click)


;; WINDOW MOVE
(bind-key* "C-S-h" 'windmove-left)
(bind-key* "C-S-n" 'windmove-right)
(bind-key* "C-S-c" 'windmove-up)
(bind-key* "C-S-t" 'windmove-down)


;; WINDOW CREATE SPLIT CLOSE
(bind-key* "M-2" 'delete-window)
(bind-key* "M-3" 'delete-other-windows)
(bind-key* "M-4" 'split-window-below)
(bind-key* "M-$" 'split-window-right)
(bind-key* "M-@" 'balance-windows)
(bind-key* "M-#" 'split-3-windows-horizontally-evenly)


;; WINDOW SHRINK, WINDOW INCREASE
(bind-key "S-<left>" 'shrink-window-horizontally)
(bind-key "S-<right>" 'enlarge-window-horizontally)
(bind-key "S-<down>" 'shrink-window)
(bind-key "S-<up>" 'enlarge-window)


;; DRAG STUFF
(bind-key "<M-up>" 'drag-stuff-up)
(bind-key "<M-down>" 'drag-stuff-down)
(bind-key "<M-left>" 'drag-stuff-left)
(bind-key "<M-right>" 'drag-stuff-right)



(custom-set-variables
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(auto-save-default nil)
 '(auto-save-interval 0)
 '(blink-cursor-mode nil)
 '(browse-url-browser-function (quote browse-url-chromium))
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("08851585c86abcf44bb1232bced2ae13bc9f6323aeda71adfa3791d6e7fea2b6" "01d299b1b3f88e8b83e975484177f89d47b6b3763dfa3297dc44005cd1c9a3bc" "c3c0a3702e1d6c0373a0f6a557788dfd49ec9e66e753fb24493579859c8e95ab")))
 '(custom-theme-load-path (quote ("~/.emacs.d/themes" t)))
 '(delete-selection-mode t)
 '(exec-path
   (quote
    ("/home/ubuntu/.cabal/bin" "/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/sbin" "/bin" "/usr/games" "/usr/local/games" "/usr/local/libexec/emacs/24.4/x86_64-unknown-linux-gnu")))
 '(recentf-menu-before nil)
 '(recentf-mode t)
 '(shift-select-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#bf616a")
     (40 . "#DCA432")
     (60 . "#ebcb8b")
     (80 . "#B4EB89")
     (100 . "#89EBCA")
     (120 . "#89AAEB")
     (140 . "#C189EB")
     (160 . "#bf616a")
     (180 . "#DCA432")
     (200 . "#ebcb8b")
     (220 . "#B4EB89")
     (240 . "#89EBCA")
     (260 . "#89AAEB")
     (280 . "#C189EB")
     (300 . "#bf616a")
     (320 . "#DCA432")
     (340 . "#ebcb8b")
     (360 . "#B4EB89"))))
 '(vc-annotate-very-old-color nil))

;; theme and font
(set-default-font "DejaVu Sans Mono 9")
(load-theme 'molokai)

(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "royal blue"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "firebrick"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "forest green"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "dark magenta"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "gold3"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "DarkOrange3"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "magenta")))))
