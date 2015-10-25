(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-verbose t)
(require 'use-package)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)


(use-package helm
  :ensure t
  :diminish helm-mode
  :init (progn
          (require 'helm-config)
          (bind-key "C-c h" #'helm-command-prefix)
          (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
                helm-input-idle-delay 0.01  ; this actually updates things
                helm-yas-display-key-on-candidate t
                helm-candidate-number-limit 100
                helm-quick-update t
                helm-M-x-requires-pattern nil
                helm-M-x-fuzzy-match t
                helm-ff-skip-boring-files t
                helm-move-to-line-cycle-in-source t
                helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-match t
                helm-locate-fuzzy-match t
                helm-split-window-in-side-p t
                helm-scroll-amount 8
                helm-autoresize-mode 1
                helm-boring-buffer-regexp-list
                (quote
                 ("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf" "\\*.*\\*")))
          (setq helm-c-source-swoop-match-functions
                '(helm-mm-exact-match
                  helm-mm-match
                  ;;helm-fuzzy-match
                  ;;helm-mm-3-migemo-match
                  ))
          (helm-mode))
  :config
  (progn
    (bind-key  "<tab>" #'helm-execute-persistent-action helm-map) ; rebind tab to do persistent action
    (bind-key  "C-i" #'helm-execute-persistent-action helm-map) ; make TAB works in terminal
    (bind-key  "C-z" #'helm-select-action helm-map) ; list actions using C-z
    (bind-key  "M-c" #'helm-previous-line helm-map)
    (bind-key  "M-t" #'helm-next-line helm-map)
    (bind-key  "M-o" #'helm-next-source helm-map)
    (bind-key  "M-C" #'helm-previous-page helm-map)
    (bind-key  "M-T" #'helm-next-page helm-map)
    (bind-key  "M-b" #'helm-beginning-of-buffer helm-map)
    (bind-key  "M-B" #'helm-end-of-buffer helm-map)
    (bind-key  "M-l" #'helm-find-files-up-one-level helm-find-files-map)
    (bind-key  "M-L" #'helm-find-files-down-last-level helm-find-files-map)
    (bind-key  "M-C" #'helm-previous-page helm-find-files-map)
    (bind-key  "M-B" #'helm-end-of-buffer helm-find-files-map)
    (bind-key  "C-f" #'helm-ff-run-find-sh-command helm-find-files-map)
    (bind-key  "C-S-f" #'helm-ff-run-locate helm-find-files-map)
    (bind-key  "C-S-d" #'helm-buffer-run-kill-persistent helm-buffer-map)
    (bind-key  "C-d" #'helm-buffer-run-kill-buffers helm-buffer-map)
    (bind-key  "M-SPC" #'helm-toggle-visible-mark helm-map)))


(use-package helm-swoop
  :ensure t :defer t
  :init (progn
          (setq helm-c-source-swoop-search-functions
                '(helm-mm-exact-search
                  helm-mm-search
                  helm-candidates-in-buffer-search-default-fn
                  ;;helm-fuzzy-search
                  ;;helm-mm-3-migemo-search
                  ))))

(use-package helm-projectile
  :ensure t :defer t)

(use-package helm-css-scss
  :ensure t :defer t)

(use-package company
  :ensure t
  :init (progn
          ;; (global-company-mode 1)
          (add-hook 'prog-mode-hook 'company-mode)
          (add-hook 'js3-mode-hook 'company-mode)
          (add-hook 'html-mode-hook 'company-mode)
          (add-hook 'css-mode-hook 'company-mode)
          (add-hook 'scss-mode-hook 'company-mode)
          (setq company-tooltip-limit 20
                company-idle-delay 0.1
                company-echo-delay 0   
                company-show-numbers t  
                company-minimum-prefix-length 1)
          (company-quickhelp-mode 1)
          (push 'company-robe company-backends))
  :config (progn
            (bind-key "<tab>" #'company-complete company-active-map)
            (bind-key "M-/" #'company-show-doc-buffer company-active-map)
            (bind-key "C-c C-d" #'company-show-doc-buffer company-active-map)
            (bind-key "C-c d" #'company-show-doc-buffer company-active-map)
            (bind-key "C-c C-l" #'company-show-location company-active-map)
            (bind-key "C-c l" #'company-show-location company-active-map)
            (bind-key "C-i" #'yas-expand company-active-map)))

(use-package company-quickhelp
  :ensure t :defer t)

(use-package eldoc
  :ensure t :defer t
  :config (progn
            (add-hook 'prog-mode-hook 'eldoc-mode)))

(use-package magit
:ensure t :defer t)

(use-package projectile
  :ensure t :defer t
  :init(progn
         (setq projectile-completion-system 'default
               projectile-enable-caching t
               projectile-completion-system 'helm
               projectile-switch-project-actian 'helm-projectile-find-file
               projectile-use-native-indexing t)
         (projectile-global-mode)))


(use-package yasnippet
  :ensure t :defer t
  :diminish yas-minor-mode
  :init(progn
         (yas-global-mode 1)
         (define-key yas-minor-mode-map [(tab)] nil)
         ;; (define-key yas-minor-mode-map (kbd "TAB") nil)
         (define-key yas-minor-mode-map (kbd "<tab>") nil)
         (bind-key "C--" #'yas-expand yas-minor-mode-map)))


(use-package molokai-theme
  :ensure t :defer t)

(use-package grandshell-theme
  :ensure t :defer t)

(use-package smartparens
  :ensure t :defer t
  :init (progn
          (smartparens-global-mode t)
          (show-smartparens-global-mode t))
  :config (use-package smartparens-config))


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

(use-package buffer-move
  :ensure t)

(use-package popwin
  :ensure t
  :config(progn
           (popwin-mode 1)))


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
  :ensure t :defer t)

(use-package goto-chg
  :ensure t :defer t)

(use-package neotree
  :ensure t :defer t
  :config(progn
           (defun neotree-enter-in-place ()
             (interactive)
             (neotree-enter)
             (neotree-show))
           (bind-key "<tab>" #'neotree-enter neotree-mode-map)
           (bind-key "e" #'neotree-enter-in-place neotree-mode-map)))


(use-package nyan-mode
  :ensure t :defer t
  :init(progn
         (add-hook 'prog-mode-hook #'nyan-mode)))


(use-package multiple-cursors
  :ensure t :defer t
  :diminish multiple-cursors-mode)


(use-package phi-search
  :ensure t :defer t
  :config (progn
            (bind-key "," #'phi-search-again-or-previous phi-search-default-map)
            (bind-key "." #'phi-search-again-or-next phi-search-default-map)))


(use-package dired+
  :ensure t
  :config(progn
           (diredp-toggle-find-file-reuse-dir t)
           (unbind-key "M-c" dired-mode-map)
           (bind-key "M-C" #'scroll-up dired-mode-map)
           (bind-key "M-T" #'scroll-down dired-mode-map)
           (bind-key "M-b" #'my-beginning-or-end-of-buffer dired-mode-map)
           (bind-key "M-B" #'my-end-or-beginning-of-buffer dired-mode-map)))


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
  :ensure t :defer t
  :config(progn
           (bind-key "C-p" #'helm-css-scss css-mode-map)))

(use-package scss-mode
  :ensure t :defer t
  :init(progn
         (setq scss-compile-at-save nil))
  :config(progn
           (bind-key "C-p" #'helm-css-scss scss-mode-map)))

(use-package markdown-mode
  :ensure t :defer t
  :init(progn
         (setq markdown-xhtml-standalone-regexp "")))

(use-package json-mode
  :ensure t :defer t)

(use-package yaml-mode
  :ensure t :defer t)

(use-package inf-mongo
  :ensure t :defer t
  :init(progn
         (setq inf-mongo-command "mongo")))

(use-package imenu
  :defer t
  :init(progn
         (setq imenu-auto-rescan t
               imenup-ignore-comments-flag nil
               imenup-sort-ignores-case-flag nil)))

(use-package semantic
  :defer t
  :init(progn
         (semantic-mode 1)))

(use-package undo-tree
  :ensure t :defer t
  :init (progn
          (global-undo-tree-mode)))

(use-package ido
  :defer t
  :init(progn
         (setq ido-enable-flex-matching t
               ibuffer-saved-filter-groups
               (quote (("default"
                        ("dired" (mode . dired-mode))
                        ("code" (or
                                 (mode . python-mode)
                                 (mode . ruby-mode)
                                 (mode . c-mode-common-hook)
                                 (mode . clojure-mode)
                                 (mode . haskell-mode)
                                 (mode . php-mode)
                                 (mode . emacs-lisp-mode)
                                 (mode . js3-mode)
                                 (mode . coffee-mode)))
                        ("mark" (or
                                 (mode . html-mode)
                                 (mode . web-mode)
                                 (mode . jinja2-mode)
                                 (mode . scss-mode)
                                 (mode . css-mode)
                                 (mode . json-mode)
                                 (mode . xml-mode)
                                 (mode . yaml-mode)
                                 (mode . markdown-mode)))
                        ("files" (name . "^[^\*].*[^\*]$"))
                        ("other" (name . "^\*[^Hh].*\*$"))
                        ("junk" (name . "^\*[Hh]elm.*\*$"))))))
         (add-hook 'ibuffer-mode-hook
                   (lambda ()
                     (ibuffer-switch-to-saved-filter-groups "default")))))


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
          (setq org-CUA-compatible nil
                org-src-preserve-indentation t
                org-pretty-entities t
                org-pretty-entities-include-sub-superscripts t
                org-startup-truncated t
                org-replace-disputed-keys nil
                org-confirm-babel-evaluate nil
                org-src-fontify-natively t
                org-src-tab-acts-natively t
                org-babel-clojure-backend 'cider
                org-babel-load-languages
                  (quote
                   ((ruby . t)
                    (clojure . t)
                    (sh . t)
                    (python . t)
                    (emacs-lisp . t)))
                org-confirm-babel-evaluate nil
                org-babel-python-command "python3"))
  :config (progn
            (unbind-key "C-e" org-mode-map)))


(use-package ob-mongo
  :ensure t :defer t)

;; PYTHON
(use-package python
  :ensure t :defer t
  :mode ("\\.py\\'" . python-mode)
  :init (progn
          (setq expand-region-preferred-python-mode (quote fgallina-python)
                python-shell-interpreter "ipython3"
                python-indent-offset 4
                python-check-command nil)))


(use-package elpy
  :ensure t :defer t
  :init(progn
         (elpy-enable)
         (setq elpy-rpc-python-command "python3"
               elpy-rpc-backend "jedi"
               elpy-modules (quote(elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-sane-defaults))))
  :config(progn
           (bind-key "C-c C-i" #'elpy-importmagic-fixup elpy-mode-map)
           (bind-key "C-c i" #'elpy-importmagic-fixup elpy-mode-map)
           (bind-key "C-c C-n" #'elpy-goto-definition elpy-mode-map)
           (bind-key "C-c n" #'elpy-goto-definition elpy-mode-map)))

(use-package jedi
  :ensure t :defer t)

(use-package pyvenv
  :ensure t :defer t
  :init(progn
         (setq pyvenv-virtualenvwrapper-python "/usr/bin/python3")))


;; RUBY
(use-package ruby-mode
  :defer t
  :config(progn
           (setq inf-ruby-default-implementation "pry")
           (bind-key "<f8>" #'inf-ruby ruby-mode-map)
           (bind-key "<f9>" #'robe-start ruby-mode-map)
           (bind-key "C-c C-c" #'ruby-send-last-sexp ruby-mode-map)))

(use-package robe
  :ensure t :defer t
  :init (progn
          (add-hook 'ruby-mode-hook 'robe-mode)))


;; JAVASCRIPT
(use-package js3-mode
  :ensure t :defer t
  :init(progn
         (setq js3-auto-indent-p t
               js3-enter-indents-newline t
               js3-indent-on-enter-key t)))


;; COFFEESCRIPT
(use-package coffee-mode
  :ensure t :defer t
  :init(progn
         (setq coffee-tab-width 2)))


(use-package tern
  :ensure t
  :init(progn
         (add-hook 'js3-mode-hook (lambda () (tern-mode t)))
         (add-hook 'coffee-mode-hook (lambda () (tern-mode t)))))


(use-package company-tern
  :ensure t
  :init (add-to-list 'company-backends 'company-tern))


;; CLOJURE
(use-package cider
  :ensure t :defer t)


(use-package clojure-mode
  :defer t
  :config
  (defun my/clojure-mode-defaults ()
    (bind-key "<f8>" 'cider-jack-in))
  (add-hook 'clojure-mode-hook 'my/clojure-mode-defaults))


;; LUA
(use-package lua-mode
  :ensure t :defer t
  :init(progn
         (setq lua-indent-level 2
               lua-prefix-key "C-c"))
  :config(progn
           (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
           (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))))


;; HASKELL
(use-package haskell-mode
  :ensure t :defer t
  :init(progn
         (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
         (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
         (add-hook 'haskell-mode-hook 'turn-on-hi2)
         (setq haskell-process-suggest-remove-import-lines t
               haskell-process-auto-import-loaded-modules t
               haskell-process-log t
               haskell-process-type 'cabal-repl))
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


(use-package ghc
  :ensure t :defer t
  :init (progn
          (autoload 'ghc-init "ghc" nil t)
          (autoload 'ghc-debug "ghc" nil t)
          (add-hook 'haskell-mode-hook (lambda () (ghc-init)))))


;;C C++
(use-package c-mode-common-hook
  :defer t
  :init(progn
         (setq-default c-basic-offset 4))
  :config(progn(
                (unbind-key "C-c C-d" c-mode-common-map)
                (unbind-key "C-c d" c-mode-common-map)
                (unbind-key "M-q" c-mode-common-map))))


(use-package irony
  :ensure t :defer t
  :config(progn
           (add-hook 'c++-mode-hook 'irony-mode)
           (add-hook 'c-mode-hook 'irony-mode)))


(use-package company-irony
  :ensure t :defer t
  :init(add-to-list 'company-backends 'company-irony))


;;PHP ;; deactive irony mode c mode hook to make it work for some obscure reason
(use-package php-mode
  :ensure t :defer t)

(use-package php-extras
  :ensure t :defer t)



;; STUFF
;; remove window decoration
(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (set-face-inverse-video-p 'vertical-border nil)
  (scroll-bar-mode -1))



;; bookmark startup
(setq inhibit-splash-screen t)
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")

;; replace yes to y 
(fset 'yes-or-no-p 'y-or-n-p)

;; tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; insert ret if last line
(setq next-line-add-newlines t)

;; scrath message
(setq initial-scratch-message nil)

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


(defun xah-run-current-file ()
  (interactive)
  (let* (
         (ξsuffix-map
          ;; (‹extension› . ‹shell program name›)
          `(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python3")
            ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
            ("rb" . "ruby")
            ("js" . "node") ; node.js
            ("sh" . "bash")
            ("clj" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
            ("ml" . "ocaml")
            ("vbs" . "cscript")
            ("tex" . "pdflatex")
            ("latex" . "pdflatex")
            ("java" . "javac")
            ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
            ))
         (ξfname (buffer-file-name))
         (ξfSuffix (file-name-extension ξfname))
         (ξprog-name (cdr (assoc ξfSuffix ξsuffix-map)))
         (ξcmd-str (concat ξprog-name " \""   ξfname "\"")))
    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified. Do you want to save first?")
        (save-buffer)))
    (cond
     ((string-equal ξfSuffix "el") (load ξfname))
     ((string-equal ξfSuffix "java")
      (progn
        (shell-command ξcmd-str "*xah-run-current-file output*" )
        (shell-command
         (format "java %s" (file-name-sans-extension (file-name-nondirectory ξfname))))))
     (t (if ξprog-name
            (progn
              (message "Running…")
              (shell-command ξcmd-str "*xah-run-current-file output*" ))
          (message "No recognized program file suffix for this file."))))))


;; shell buffer
(defun my-filter-shell (condp lst)
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))
(defun shell-dwim (&optional create)
  (interactive "P")
  (let ((next-shell-buffer) (buffer)
        (shell-buf-list (identity ;;used to be reverse                                                                         
                         (sort
                          (my-filter-shell (lambda (x) (string-match "^\\*shell\\*" (buffer-name x))) (buffer-list))
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


(defun shell-buffer ()
  (interactive)
  (switch-to-buffer "*Shell Command Output*"))

(defun python-buffer ()
  (interactive)
  (switch-to-buffer "*Python*"))

(defun split-3-3-0 ()
  (interactive)
  (delete-other-windows)
  (command-execute 'split-window-horizontally)
  (command-execute 'split-window-horizontally)
  (command-execute 'balance-windows))

(defun split-3-2-1 ()
  (interactive)
  (delete-other-windows)
  (command-execute 'split-window-vertically)
  (command-execute 'split-window-horizontally)
  (enlarge-window 20))

(defun split-6-3-1 ()
  (interactive)
  (delete-other-windows)
  (command-execute 'split-window-vertically)
  (command-execute 'split-window-horizontally)
  (command-execute 'split-window-horizontally)
  (command-execute 'balance-windows)
  (enlarge-window 20))

(defun split-6-3-3 ()
  (interactive)
  (delete-other-windows)
  (command-execute 'split-window-vertically)
  (command-execute 'split-window-horizontally)
  (command-execute 'split-window-horizontally)
  (windmove-down)
  (command-execute 'split-window-horizontally)
  (command-execute 'split-window-horizontally)
  (command-execute 'balance-windows)
  (windmove-up)
  (enlarge-window 20))

(defun smart-ret()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun smart-ret-reverse()
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (indent-for-tab-command))


;; there is a bug with multiple-cursors and ergoemacs-cut-line-or-region 
(defun my-cut-line-or-region ()
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))

(defun my-copy-line-or-region (&optional arg)
  "Copy current line, or current text selection."
  (interactive "P")
  (cond
   ((and (boundp 'cua--rectangle) cua--rectangle cua-mode)
    (cua-copy-rectangle arg))
   ((and (region-active-p) cua-mode)
    (cua-copy-region arg))
   ((region-active-p)
    (kill-ring-save (region-beginning) (region-end)))
   (t
    (kill-ring-save
     (save-excursion
       (let ((pt (point)))
         (when (= pt (point))
           (call-interactively 'move-beginning-of-line)))
       (when (not (bolp))
         (beginning-of-line))
       (point))
     (save-excursion
       (let ((pt (point)))
         (when (= pt (point))
           (call-interactively 'move-end-of-line)))
       (re-search-forward "\\=\n" nil t) ;; Include newline
       (point)))))
  (deactivate-mark))

(defun join-line-or-lines-in-region ()
  (interactive)
  (cond ((region-active-p)
         (let ((min (line-number-at-pos (region-beginning))))
           (goto-char (region-end))
           (while (> (line-number-at-pos) min)
             (join-line))))
        (t (call-interactively 'join-line))))

(defun duplicate-current-line-or-region (arg)
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and (region-active-p) (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if (region-active-p)
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun push-mark-no-activate ()
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  (interactive)
  (set-mark-command 1))

(defun my-forward-block (&optional number)
  (interactive "p")
  (if (and number
           (> 0 number))
      (ergoemacs-backward-block (- 0 number))
    (if (search-forward-regexp "\n[[:blank:]\n]*\n+" nil "NOERROR" number)
        (progn (backward-char))
      (progn (goto-char (point-max))))))

(defun my-backward-block (&optional number)
  (interactive "p")
  (if (and number
           (> 0 number))
      (ergoemacs-forward-block (- 0 number))
    (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR" number)
        (progn
          (skip-chars-backward "\n\t ")
          (forward-char 1))
      (progn (goto-char (point-min))))))

(defun my-beginning-of-line-or-block (&optional n)
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (if (equal n 1)
        (if (or (equal (point) (line-beginning-position))
                (equal last-command this-command))
            (my-backward-block n)
          (beginning-of-line)
          (back-to-indentation))
      (my-backward-block n))))

(defun my-end-of-line-or-block (&optional n)
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (if (equal n 1)
        (if (or (equal (point) (line-end-position))
                (equal last-command this-command))
            (my-forward-block)
          (end-of-line))
      (progn (my-forward-block n)))))

(defun my-select-current-line ()
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(defun my-select-current-block ()
  (interactive)
  (let (p1)
    (if (re-search-backward "\n[ \t]*\n" nil "move")
        (progn (re-search-forward "\n[ \t]*\n")
               (setq p1 (point)))
      (setq p1 (point)))
    (if (re-search-forward "\n[ \t]*\n" nil "move")
        (re-search-backward "\n[ \t]*\n"))
    (set-mark p1)))

(defun my-kill-line-backward (arg)
  (interactive "p")
  (kill-line (- 1 arg))
  (indent-for-tab-command))

(defun my-toggle-letter-case (φp1 φp2)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let ((ξbds (bounds-of-thing-at-point 'word)))
       (list (car ξbds) (cdr ξbds)))))
  (let ((deactivate-mark nil))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region φp1 φp2)
      (put this-command 'state 1))
     ((equal 1  (get this-command 'state))
      (upcase-region φp1 φp2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region φp1 φp2)
      (put this-command 'state 0)))))

(defun my-new-empty-buffer ()
  (interactive)
  (let ((ξbuf (generate-new-buffer "untitled")))
    (switch-to-buffer ξbuf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

(defun my-previous-user-buffer ()
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
      (setq i (1+ i)) (previous-buffer) )))

(defun my-next-user-buffer ()
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
      (setq i (1+ i)) (next-buffer) )))


(defun my-indent-shift-left (start end &optional count)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count python-indent-offset))
  (when (> count 0)
    (let ((deactivate-mark nil))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (if (and (< (current-indentation) count)
                   (not (looking-at "[ \t]*$")))
              (error "Can't shift all lines enough"))
          (forward-line))
        (indent-rigidly start end (- count))))))


(defun my-indent-shift-right (start end &optional count)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let ((deactivate-mark nil))
    (setq count (if count (prefix-numeric-value count)
                  python-indent-offset))
    (indent-rigidly start end count)))



;; KEYBOARD
;; MARK COMMAND, COMPLETE, YAS, TAB
(bind-key "M-SPC" 'set-mark-command)
(bind-key "C-SPC" 'company-complete)
(bind-key "TAB" 'indent-for-tab-command)
(bind-key "<backtab>" 'my-indent-shift-left)

(bind-key* "C-a" 'mark-whole-buffer)
(bind-key* "<M-return>" 'smart-ret)
(bind-key* "<S-return>" 'smart-ret-reverse)
(bind-key "<escape>" 'keyboard-espace-quit)
(bind-key "M-m" 'emmet-expand-line)
(bind-key "C-x j" 'dired-jump)
(bind-key "<f2>" 'neotree-toggle)
(bind-key* "C-t" 'jump-to-mark)
(bind-key "C-x t" 'push-mark-no-activate)
(bind-key "C-c g" #' magit-status)
(bind-key "C-c C-g" #' magit-tatus)
(bind-key "C-c M-g" #' magit-dispatch-popup)
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
(bind-key* "M-G" 'my-backward-block)
(bind-key* "M-R" 'my-forward-block)
(bind-key* "M-d" 'my-beginning-of-line-or-block)
(bind-key* "M-D" 'my-end-of-line-or-block)
(bind-key* "M-b" 'beginning-of-buffer)
(bind-key* "M-B" 'end-of-buffer)


;; SMARTPARENS
(bind-key* "M-H" 'sp-backward-sexp)
(bind-key* "M-N" 'sp-forward-sexp)
(bind-key* "M-9" 'sp-splice-sexp)
(bind-key* "M-[" 'sp-forward-slurp-sexp)
(bind-key* "M-]" 'sp-backward-slurp-sexp)
(bind-key* "M-{" 'sp-backward-barf-sexp)
(bind-key* "M-}" 'sp-forward-barf-sexp)
(bind-key "C-S-j" 'sp-join-sexp)
(bind-key "C-k" 'sp-backward-kill-sexp)
(bind-key "C-S-k" 'sp-kill-sexp)
(bind-key "C-c c-t" 'sp-transpose-hybrid-sexp)
(bind-key "C-c t" 'sp-transpose-hybrid-sexp)

;; DELETE KEY
(bind-key* "M-e" 'backward-delete-char-untabify)
(bind-key* "M-u" 'delete-char)
(bind-key* "M-." 'backward-kill-word)
(bind-key* "M-p" 'kill-word)
(bind-key* "M-i" 'kill-line)
(bind-key* "M-I" 'my-kill-line-backward)
(bind-key* "M-y" 'delete-indentation)

;; COPY, CUT, PASTE, REDO, UNDO
(bind-key* "M-q" 'my-cut-line-or-region)
(bind-key* "M-j" 'my-copy-line-or-region)
(bind-key* "M-k" 'yank)
(bind-key* "M-;" 'undo-tree-undo)
(bind-key* "M-:" 'undo-tree-redo)
(bind-key "C-z" 'undo-tree-undo)
(bind-key* "C-S-z" 'undo-tree-redo)

;; POP, SAVE, GOTO, INFO, SCALE, CAMEL, RECENTER, REPLACE
(bind-key* "M-f" 'goto-last-change)
(bind-key* "M-F" 'goto-last-change-reverse)
(bind-key* "C-s" 'save-buffer)
(bind-key* "C-S-s" 'ido-write-file)
(bind-key* "C-l" 'goto-line)
(bind-key* "C-/" 'helm-info-at-point)
(bind-key* "C-=" 'text-scale-increase)
(bind-key* "C-+" 'text-scale-decrease)
(bind-key* "M-z" 'my-toggle-letter-case)
(bind-key* "M-*" 'replace-regexp)

;; NEW BUFFER, FRAME CLOSE BUFFER, COMMENT
(bind-key "C-n" 'my-new-empty-buffer)
(bind-key* "C-b" 'make-frame-command)
(bind-key* "C-w" 'kill-this-buffer)
(bind-key* "M--" 'comment-dwim)

;; COMMAND, SHELL, RUN, EMMET
(bind-key* "M-a" 'helm-M-x)
(bind-key* "M-A" 'shell-command)
(bind-key* "M-C-A" 'eval-expression)
(bind-key* "M-1" 'shell-dwim)
(bind-key* "<f1>" 'shell-buffer)
(bind-key* "<f5>" 'xah-run-current-file)
(bind-key* "<f6>" 'helm-recentf)
(bind-key* "<f7>" 'helm-bookmarks)
(bind-key* "<f12>" 'toggle-frame-fullscreen) 
(bind-key* "C-o" 'helm-find-files)
(bind-key "C-p" 'helm-semantic-or-imenu)
(bind-key "C-y" 'helm-show-kill-ring)
(bind-key "C-f" 'helm-projectile-switch-to-buffer)
(bind-key "C-S-f" 'helm-locate)
(bind-key "C-h a" 'helm-apropos)
(global-set-key (kbd "M-o") 'helm-projectile-find-file)
(global-set-key (kbd "C-e") 'helm-buffers-list)

;; HELM SWOOP
(bind-key* "C-r" 'helm-swoop)
(bind-key* "C-S-r" 'helm-swoop-back-to-last-point)
(bind-key* "M-7" 'helm-multi-swoop)
(bind-key* "M-8" 'helm-multi-swoop-all)

;; SELECTION
(bind-key "M-l" 'my-select-current-line)
(bind-key "M-L" 'my-select-current-block)
(bind-key* "M-S" 'er/mark-inside-pairs)
(bind-key* "M-s" 'er/expand-region)

;; BUFFER SWITCHING ENANCEMENT
(bind-key* "M-'" 'my-previous-user-buffer)
(bind-key* "M-," 'my-next-user-buffer)
(bind-key* "M-<" 'register-to-point)
(bind-key* "M-\"" 'point-to-register)
(bind-key* "C-S-e" 'ibuffer)

;; MULTIPLE CURSORS
(bind-key "C-d" 'duplicate-current-line-or-region)
(bind-key "C-j" 'join-line-or-lines-in-region)
(unbind-key "M-<down-mouse-1>")
(bind-key* "M-<mouse-1>" 'mc/add-cursor-on-click)

;; WINDOW MOVE
(bind-key* "C-S-h" 'windmove-left)
(bind-key* "C-S-n" 'windmove-right)
(bind-key* "C-S-c" 'windmove-up)
(bind-key* "C-S-t" 'windmove-down)

;; WINDOW SWITCH
(bind-key* "C-S-M-h" 'buf-move-left)
(bind-key* "C-S-M-n" 'buf-move-right)
(bind-key* "C-S-M-c" 'buf-move-up)
(bind-key* "C-S-M-t" 'buf-move-down)

;; WINDOW CREATE SPLIT CLOSE
(bind-key* "M-2" 'delete-window)
(bind-key* "M-3" 'delete-other-windows)
(bind-key* "M-4" 'split-window-below)
(bind-key* "M-$" 'split-window-right)
(bind-key* "M-@" 'balance-windows)
(bind-key* "M-5" 'split-3-2-1)
(bind-key* "M-%" 'split-6-3-3)
(bind-key* "M-6" 'split-6-3-1)
(bind-key* "M-^" 'split-3-3-0)

;; WINDOW SHRINK, WINDOW INCREASE
(bind-key* "S-<left>" 'shrink-window-horizontally)
(bind-key* "S-<right>" 'enlarge-window-horizontally)
(bind-key* "S-<down>" 'shrink-window)
(bind-key* "S-<up>" 'enlarge-window)

;; DRAG STUFF
(bind-key "<M-up>" 'drag-stuff-up)
(bind-key "<M-down>" 'drag-stuff-down)
(bind-key "<M-left>" 'drag-stuff-left)
(bind-key "<M-right>" 'drag-stuff-right)

;; LISP
(bind-key "C-c C-c" 'eval-defun emacs-lisp-mode-map)
(bind-key "C-c C-r" 'eval-region emacs-lisp-mode-map)
(bind-key "C-c C-b" 'eval-buffer emacs-lisp-mode-map)
(bind-key "C-c C-e" 'eval-last-sexp emacs-lisp-mode-map)
(bind-key "C-c e" 'eval-last-sexp emacs-lisp-mode-map)
(bind-key "C-c C-f" 'eval-last-sexp emacs-lisp-mode-map)


(use-package region-bindings-mode
  :ensure t
  :diminish region-bindings-mode
  :config(progn
           (region-bindings-mode-enable)
           (bind-key "h" #'mc/mark-previous-like-this region-bindings-mode-map)
           (bind-key "n" #'mc/mark-next-like-this region-bindings-mode-map)
           (bind-key "H" #'mc/skip-to-previous-like-this region-bindings-mode-map)
           (bind-key "N" #'mc/skip-to-next-like-this region-bindings-mode-map)
           (bind-key "c" #'mc/unmark-previous-like-this region-bindings-mode-map)
           (bind-key "u" #'universal-argument region-bindings-mode-map)
           (bind-key "t" #'mc/unmark-next-like-this region-bindings-mode-map)
           (bind-key "a" #'mc/mark-all-like-this region-bindings-mode-map)
           (bind-key "m" #'mc/mark-more-like-this-extended region-bindings-mode-map)
           (bind-key "r" #'mc/edit-lines region-bindings-mode-map)
           (bind-key "l" #'mc/edit-beginnings-of-lines region-bindings-mode-map)
           (bind-key "/" #'mc/edit-ends-of-lines region-bindings-mode-map)
           (bind-key "f" #'mc/mark-all-in-region-regexp region-bindings-mode-map)
           (bind-key "m" #'mc/mark-sgml-tag-pair region-bindings-mode-map)
           (bind-key "." #'phi-search region-bindings-mode-map)
           (bind-key "," #'phi-search-backward region-bindings-mode-map)
           (bind-key "e" #'backward-delete-char region-bindings-mode-map)
           (bind-key "g" #'keyboard-escape-quit region-bindings-mode-map)
           (bind-key "C-g" #'keyboard-escape-quit region-bindings-mode-map)
           (bind-key "-" #'comment-dwim region-bindings-mode-map)
           (bind-key "q" #'kill-region region-bindings-mode-map)
           (bind-key "j" #'kill-ring-save region-bindings-mode-map)
           (bind-key "k" #'yank region-bindings-mode-map)
           (bind-key "x" #'kill-rectangle region-bindings-mode-map)
           (bind-key "b" #'replace-rectangle region-bindings-mode-map)
           (bind-key "D" #'duplicate-current-line-or-region region-bindings-mode-map)
           (bind-key "d" #'join-line-or-lines-in-region region-bindings-mode-map)
           (bind-key "i" #'join-line-or-lines-in-region region-bindings-mode-map)
           (bind-key "9" #'sp-splice-sexp region-bindings-mode-map)
           (bind-key "o" #'exchange-point-and-mark region-bindings-mode-map)
           (bind-key "M-o" #'exchange-point-and-mark region-bindings-mode-map)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(auto-compression-mode nil)
 '(auto-save-default nil)
 '(auto-save-interval 0)
 '(blink-cursor-mode nil)
 '(browse-url-browser-function (quote browse-url-chromium))
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("af960831c1b33b719cda2ace858641dd8accc14d51e8ffb65b39ca75f07d595d" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "8fed5e4b89cf69107d524c4b91b4a4c35bcf1b3563d5f306608f0c48f580fdf8" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "3ed645b3c08080a43a2a15e5768b893c27f6a02ca3282576e3bc09f3d9fa3aaa" "f0d8af755039aa25cd0792ace9002ba885fd14ac8e8807388ab00ec84c9497d7" "11636897679ca534f0dec6f5e3cb12f28bf217a527755f6b9e744bd240ed47e1" "50ce37723ff2abc0b0b05741864ae9bd22c17cdb469cae134973ad46c7e48044" "08851585c86abcf44bb1232bced2ae13bc9f6323aeda71adfa3791d6e7fea2b6" "01d299b1b3f88e8b83e975484177f89d47b6b3763dfa3297dc44005cd1c9a3bc" "c3c0a3702e1d6c0373a0f6a557788dfd49ec9e66e753fb24493579859c8e95ab")))
 '(delete-selection-mode 1)
 '(exec-path
   (append exec-path
           (quote
            ("/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/sbin" "/bin" "/opt/node/bin"))))
 '(package-selected-packages
   (quote
    (unicode-fonts buffer-move neotree cider-mode cider popwin elisp--witness--lisp company-irony expand-region company-quickhelp company yaml-mode windata use-package tree-mode smartparens shm scss-mode rainbow-delimiters python-info pydoc-info php-mode nyan-mode multiple-cursors molokai-theme markdown-mode lua-mode leuven-theme json-rpc json-mode js3-mode js2-mode jinja2-mode jedi iedit hi2 helm-swoop helm-projectile helm-hoogle helm-ghc helm-css-scss helm-company goto-chg fullscreen-mode framemove f emmet-mode drag-stuff dired+ company-tern company-jedi company-ghc coffee-mode auto-save-buffers-enhanced auto-compile)))
 '(ring-bell-function (quote ignore) t)
 '(same-window-buffer-names (quote ("*shell*")))
 '(scroll-error-top-bottom t)
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
;; (setq debug-on-error t)

;; theme and font
(set-default-font "DejaVu Sans Mono 9")
(load-theme 'molokai)
(load-theme 'grandshell)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "royal blue"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "firebrick"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "forest green"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "dark magenta"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "gold3"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "DarkOrange3"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "magenta")))))
