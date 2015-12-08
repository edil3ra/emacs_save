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
                helm-move-to-line-cycle-in-source nil
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
                  helm-candidates-in-buffer-search-default-fn)
                  helm-swoop-pre-input-function (lambda () ""))))

(use-package helm-projectile
  :ensure t :defer t)

(use-package helm-css-scss
  :ensure t :defer t)

(use-package company
  :ensure t
  :init (progn
          ;; (global-company-mode 1)
          (add-hook 'prog-mode-hook 'company-mode)
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
            (bind-key "C-n" #'company-select-next company-active-map)
            (bind-key "C-h" #'company-select-previous company-active-map)
            (bind-key "C-c C-d" #'company-show-doc-buffer company-active-map)
            (bind-key "C-c d" #'company-show-doc-buffer company-active-map)
            (bind-key "C-c C-l" #'company-show-location company-active-map)
            (bind-key "C-c l" #'company-show-location company-active-map)
            (bind-key "C-h" #'company-select-previous company-active-map)
            (bind-key "C-n" #'company-select-next company-active-map)
            (unbind-key "M-h" company-active-map)
            (unbind-key "M-n" company-active-map)
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
  :ensure t :defer t :diminish projectile-mode
  :init(progn
         (setq projectile-enable-caching t
               projectile-completion-system 'helm
               ;; projectile-switch-project-action 'helm-projectile-find-file
               projectile-use-native-indexing t)
         (helm-projectile-on)
         (projectile-global-mode)))

(use-package yasnippet
  :ensure t :defer t
  :diminish yas-minor-mode
  :init(progn
         (yas-global-mode 1)
         (define-key yas-minor-mode-map (kbd "TAB") nil)
         (define-key yas-minor-mode-map (kbd "<tab>") nil)
         (bind-key "C--" #'yas-expand yas-minor-mode-map)))

(use-package hydra
  :ensure t :defer t)

(use-package edit-server
  :ensure t :defer t
  :init (progn
          (setq edit-server-new-frame nil)))


(use-package simple-httpd
  :ensure t :defer t)

(use-package elscreen
  :ensure t :defer t
  :init (progn
          (setq elscreen-display-screen-number t
                elscreen-display-tab nil)
          (elscreen-start)))

(use-package alpha
  :ensure t)

(use-package smartparens
  :ensure t :defer t
  :init (progn
          (smartparens-global-mode t)
          (show-smartparens-global-mode t))
  :config (progn
            (use-package smartparens-config)))

(use-package ace-jump-mode
  :ensure t :defer t)

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

(use-package smartscan
  :ensure t)

(use-package auto-save-buffers-enhanced
  :ensure t
  :init(progn
         (auto-save-buffers-enhanced t)))

(use-package recentf-mode
  :defer t :diminish recentf-mode
  :init(progn
         (recentf-mode t)))

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

(use-package beacon
  :ensure t :defer t
  :init (progn
          (beacon-mode)))

(use-package nyan-mode
 :ensure t
 :init(progn
        (add-hook 'prog-mode-hook #'nyan-mode)))

(use-package multiple-cursors
  :ensure t
  :diminish multiple-cursors-mode)

(use-package phi-search
  :ensure t)

(use-package dired+
  :ensure t :defer t
  :init (progn
          (diredp-toggle-find-file-reuse-dir t))
  :config(progn
           (unbind-key "M-c" dired-mode-map)
           (bind-key "M-C" #'scroll-down-command dired-mode-map)
           (bind-key "M-T" #'scroll-up-command dired-mode-map)
           (bind-key "M-b" #'beginning-of-buffer dired-mode-map)
           (bind-key "M-B" #'end-of-buffer dired-mode-map)))



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
          (global-undo-tree-mode)
          (setq undo-tree-visualizer-timestamps t)))

;; THEMES

(use-package grandshell-theme
  :ensure t :defer t)
(use-package zenburn-theme
  :ensure t :defer t)
(use-package gotham-theme
  :ensure t :defer t)
(use-package monokai-theme
  :ensure t :defer t)
(use-package noctilux-theme
  :ensure t :defer t)

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
                                 (mode . js2-mode)
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
(use-package js2-mode
  :ensure t :defer t
  :mode ("\\.js\\'" . js2-mode)
  :init (progn
          (setq js2-basic-offset 4)))


;; COFFEESCRIPT
(use-package coffee-mode
  :ensure t :defer t
  :init(progn
         (setq coffee-tab-width 2)))

;; TERN
(use-package tern
  :ensure t
  :init(progn
         (add-hook 'js-mode-hook (lambda () (tern-mode t)))
         (add-hook 'coffee-mode-hook (lambda () (tern-mode t))))
  :config(progn
           (unbind-key "C-c C-c" tern-mode-keymap)
           (defun delete-tern-process ()
             (interactive)
             (delete-process "Tern"))))


;; JS2-REFACTOR
(defhydra hydra-js2-refactor (:hint nil)
  "
 ^Function^          ^Variables^       ^Contract^          ^Struct^          ^Misc^
 ╭───────────────────────────────────────────────────────────────────────────────────────╯
 [_ef_] extract f    [_ev_] extract    [_cu_] contract f   [_ti_] ternary    [_lt_] log  
 [_em_] extract m    [_iv_] inline     [_eu_] expand f     [_uw_] unwrap     [_sl_] slurp
 [_ip_] extract ip   [_rv_] rename     [_ca_] contract a   [_ig_] inject g   [_ba_] barf
 [_lp_] extract lp   [_vt_] var-this   [_ea_] expand a     [_wi_] wrap b       
 [_ao_] args-obj     [_sv_] split      [_co_] contract o
  ^ ^                ^ ^               [_eo_] contract o
"
  ("ef" js2r-extract-function)
  ("em" js2r-extract-method)
  ("ip" js2r-introduce-parameter)
  ("lp" js2r-localize-parameter)
  ("ao" js2r-arguments-to-object)
  ("ev" js2r-extract-var)
  ("iv" js2r-inline-var)
  ("rv" js2r-rename-var)
  ("vt" js2r-var-to-this)
  ("sv" js2r-split-var-declaration)
  ("cu" js2r-contract-function)
  ("eu" js2r-expand-function)
  ("ca" js2r-contract-array)
  ("ea" js2r-expand-array)
  ("co" js2r-contract-object)
  ("eo" js2r-expand-object)
  ("ti" js2r-ternary-to-if)
  ("uw" js2r-unwrap)
  ("ig" js2r-inject-global-in-iife)
  ("wi" js2r-wrap-buffer-in-iife)
  ("lt" js2r-log-this)
  ("sl" js2r-forward-slurp)
  ("ba" js2r-forwaqrd-barf)
  ("q" nil))

(use-package js2-refactor
  :ensure t :defer
  :init(progn
         (add-hook 'js2-mode-hook #'js2-refactor-mode))
  :config(progn
           (bind-key "C-c r" 'hydra-js2-refactor/body js2-refactor-mode-map)))



(use-package company-tern
  :ensure t
  :init (add-to-list 'company-backends 'company-tern))

;; SKEWER
(use-package skewer-mode
  :ensure t :defer t
  :init(progn
         ;; (add-hook 'js2-mode-hook 'skewer-mode)
         (add-hook 'css-mode-hook 'skewer-css-mode)
         (add-hook 'html-mode-hook 'skewer-html-mode))
  :config(progn
           (bind-key "C-c C-c" #'skewer-eval-defun skewer-mode-map)
           (bind-key "C-c c" #'skewer-eval-defun skewer-mode-map)
           (bind-key "C-c C-k" #'skewer-load-buffer skewer-mode-map)
           (bind-key "C-c k" #'skewer-load-buffer skewer-mode-map)
           (add-hook 'skewer-css-mode-hook (lambda ()
                                             (bind-key "C-c C-c" #'skewer-css-eval-current-rule skewer-css-mode-map)
                                             (bind-key "C-c C-r" #'skewer-css-clear-all skewer-css-mode-map)))))



;; CLOJURE
(use-package clojure-mode
  :defer t
  :config
  (defun my/clojure-mode-defaults ()
    (bind-key "<f8>" 'cider-jack-in)
    (bind-key "C-c d l" 'clojure-cheatsheet))
  (add-hook 'clojure-mode-hook 'my/clojure-mode-defaults))

(use-package cider
  :ensure t :defer t)

(use-package clojure-cheatsheet
  :ensure t :defer t)

(use-package clj-refactor
  :ensure t :defer t
  :init(progn
         (setq cljr-suppress-middleware-warnings t)
         (add-hook 'clojure-mode-hook (lambda ()
                                        (clj-refactor-mode 1)
                                        (cljr-add-keybindings-with-prefix "C-c s")))))



;; COMMON LISP
(use-package slime
  :ensure t :defer t
  :init (progn
          (setq inferior-lisp-program "sbcl")))

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



;; ;; bookmark startup
;; (setq inhibit-splash-screen t)
;; (bookmark-bmenu-list)
;; (switch-to-buffer "*Bookmark List*")

;; replace yes to y
(fset 'yes-or-no-p 'y-or-n-p)
;; tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; insert ret if last line
(setq next-line-add-newlines t)

;; scratch message
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)

;; desktop mode
(desktop-save-mode)

;; save on focus out
(defun my-save-out-hook ()
    (interactive)
    (save-some-buffers t))
(add-hook 'focus-out-hook 'my-save-out-hook)

;; save all no prompt
(defun my-save-all  ()
    (interactive)
    (save-some-buffers t))


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

(defun scratch-buffer ()
  (interactive)
  (switch-to-buffer "scratch*"))

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
  (enlarge-window 18))

(defun split-6-3-1 ()
  (interactive)
  (delete-other-windows)
  (command-execute 'split-window-vertically)
  (command-execute 'split-window-horizontally)
  (command-execute 'split-window-horizontally)
  (command-execute 'balance-windows)
  (enlarge-window 18))

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
  (enlarge-window 18))

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
  (end-of-line)
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



;; GENERAL KEYBINDING
;; MARK COMMAND, COMPLETE, YAS, TAB, SAVE
(bind-key "M-SPC" 'set-mark-command)
(bind-key "C-SPC" 'company-complete)
(bind-key "TAB" 'indent-for-tab-command)
;; (bind-key "<tab>" 'indent-for-tab-command)
(bind-key "<backtab>" 'my-indent-shift-left)
(bind-key "C--" 'yas-expand)
(bind-key* "C-a" 'mark-whole-buffer)
(bind-key* "<M-return>" 'smart-ret)
(bind-key* "<S-return>" 'smart-ret-reverse)
(bind-key "<escape>" 'keyboard-espace-quit)
(bind-key "M-m" 'emmet-expand-line)
(bind-key "C-x j" 'dired-jump)
(bind-key "<f2>" 'neotree-toggle)
(bind-key "C-x t" 'push-mark-no-activate)
(bind-key "C-x s" 'save-some-buffers)
(bind-key "C-x C-s" 'save-buffer)

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
(bind-key "M-b" 'beginning-of-buffer)
(bind-key "M-B" 'end-of-buffer)
(bind-key "C-n" 'ace-jump-mode)


;; SMARTPARENS
(bind-key* "M-H" 'sp-backward-sexp)
(bind-key* "M-N" 'sp-forward-sexp)
(bind-key* "M-9" 'sp-splice-sexp)
(bind-key* "M-[" 'sp-forward-barf-sexp)
(bind-key* "M-]" 'sp-forward-slurp-sexp)
(bind-key* "M-{" 'sp-backward-barf-sexp)
(bind-key* "M-}" 'sp-backward-slurp-sexp)
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
(bind-key* "C-z" 'undo-tree-undo)
(bind-key "C-S-z" 'undo-tree-redo)
(bind-key "C-x u" 'undo-tree-visualize)


;; POP, GOTO, INFO, SCALE, CAMEL, RECENTER, REPLACE
(bind-key* "M-f" 'goto-last-change)
(bind-key* "M-F" 'goto-last-change-reverse)
(bind-key* "C-S-s" 'ido-write-file)
(bind-key* "C-l" 'goto-line)
(bind-key* "C-/" 'helm-info-at-point)
(bind-key* "C-=" 'text-scale-increase)
(bind-key* "C-+" 'text-scale-decrease)
(bind-key* "M-z" 'my-toggle-letter-case)
(bind-key* "M-*" 'replace-regexp)

;; FRAME CLOSE BUFFER, COMMENT
(bind-key* "C-b" 'make-frame-command)
(bind-key* "C-w" 'kill-this-buffer)
(bind-key* "M--" 'comment-dwim)

;; COMMAND, SHELL, RUN, EMMET
(bind-key* "M-a" 'helm-M-x)
(bind-key* "M-A" 'shell-command)
(bind-key* "M-C-A" 'eval-expression)
(bind-key* "M-1" 'shell-dwim)
(bind-key* "S-<f1>" 'shell-buffer)
(bind-key* "<f1>" 'scratch-buffer)
(bind-key* "<f5>" 'xah-run-current-file)
(bind-key* "<f6>" 'helm-recentf)
(bind-key* "<f7>" 'helm-bookmarks)
(bind-key* "<f12>" 'toggle-frame-fullscreen)
(bind-key* "C-o" 'helm-find-files)
(bind-key "C-p" 'helm-semantic-or-imenu)
(bind-key* "C-y" 'helm-show-kill-ring)
(bind-key "C-f" 'helm-projectile-switch-to-buffer)
(bind-key "C-S-f" 'helm-locate)
(bind-key "C-h a" 'helm-apropos)
(global-set-key (kbd "M-o") 'projectile-find-file)
(global-set-key (kbd "C-e") 'helm-buffers-list)

;; HELM SWOOP
(bind-key* "C-r" 'helm-swoop)
(bind-key* "C-S-r" 'helm-swoop-back-to-last-point)
(bind-key* "M-7" 'helm-multi-swoop)
(bind-key* "M-8" 'helm-multi-swoop-all)

;; MAGIT
(bind-key "C-c g" #' magit-status)
(bind-key "C-c C-g" #' magit-status)
(bind-key "C-c M-g" #' magit-dispatch-popup)

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
(bind-key "C-c C-k" 'eval-buffer emacs-lisp-mode-map)
(bind-key "C-c C-e" 'eval-last-sexp emacs-lisp-mode-map)
(bind-key "C-c e" 'eval-last-sexp emacs-lisp-mode-map)
(bind-key "C-c C-f" 'eval-last-sexp emacs-lisp-mode-map)



;; HYDRA KEYBINDING
(defhydra hydra-window (:hint nil)
   "
    ^Movement^      ^Split^             ^Switch^        ^Resize^
  ╭──────────────────────────────────────────────────────────────────────╯
    [_h_] ←         [_r_] vertical      [_a_] ←         [_H_] X←
    [_t_] ↓         [_g_] horizontal    [_o_] ↓         [_T_] X↓
    [_c_] ↑         [_z_] undo          [_,_] ↑         [_C_] X↑
    [_n_] →         [_y_] reset         [_e_] →         [_N_] X→
    [_F_] follow    [_d_] delete        [_f_] buffer
    ^ ^             [_l_] other         [_b_] find
"
   ("h" windmove-left)
   ("t" windmove-down)
   ("c" windmove-up)
   ("n" windmove-right)
   ("F" follow-mode)
   ("r" split-window-right)
   ("g" split-window-below)
   ("z" (progn
          (winner-undo)
          (setq this-command 'winner-undo)))
   ("y" winner-redo)
   ("d" delete-window)
   ("l" delete-other-windows)
   ("a" buf-move-left)
   ("o" buf-move-down)
   ("," buf-move-up)
   ("e" buf-move-right)
   ("H" shrink-window-horizontally)
   ("C" shrink-window)
   ("T" enlarge-window)
   ("N" enlarge-window-horizontally)
   ("b" helm-mini)
   ("f" helm-find-files)
   ("1" split-3-2-1)
   ("2" split-3-3-0)
   ("3" split-6-3-1)
   ("4" split-6-3-3)
   ("q" nil))




(defhydra hydra-elscreen (:color red :hint nil)
"
  [_c_] create    [_n_] next        [_k_] kill     [_e_] helm     [_i_] show-tab
  [_C_] clone     [_h_] previous    [_K_] killB    [_d_] dired    [_b_] show-buf
  [_a_] toggle    [_t_] goto        [_s_] swap     [_l_] list
"
  ("a" elscreen-toggle)
  ("c" elscreen-create)
  ("C" elscreen-clone)
  ("k" elscreen-kill)
  ("K" elscreen-kill-screen-and-buffers)
  ("s" elscreen-swap)
  ("n" elscreen-next)
  ("h" elscreen-previous)
  ("t" elscreen-goto)
  ("e" helm-elscreen)
  ("d" elscreen-dired)
  ("i" elscreen-toggle-display-tab)
  ("b" elscreen-toggle-display-screen-number)
  ("l" elscreen-display-screen-name-list)
  ("1" (elscreen-goto 0))
  ("2" (elscreen-goto 1))
  ("3" (elscreen-goto 2))
  ("4" (elscreen-goto 3))
  ("5" (elscreen-goto 4))
  ("g" keyboard-quit)
  ("q" nil :color blue))



(defhydra hydra-yasnippet (:color amaranth :hint nil)
  "
      Modes:    Load/Visit:   Actions:
  ╭────────────────────────────────────╯
     _o_lobal  _d_irectory   _i_nsert
     _m_inor   _f_ile        _t_ryout
     _e_xtra   _l_ist        _n_ew
     _a_ll
"
  ("d" yas-load-directory)
  ("e" yas-activate-extra-mode)
  ("i" yas-insert-snippet)
  ("f" yas-visit-snippet-file :color blue)
  ("n" yas-new-snippet)
  ("t" yas-tryout-snippet)
  ("l" yas-describe-tables)
  ("o" yas/global-mode)
  ("m" yas/minor-mode)
  ("a" yas-reload-all)
  ("g" keyboard-quit)
  ("q" nil :color blue))



(defhydra hydra-project (:color teal :hint nil)
  "
    Files             Search          Buffer             Do         │ Projectile │
  ╭─────────────────────────────────────────────────────────────────┴────────────╯
    [_f_] file          [_a_] ag          [_e_] switch         [_g_] magit
    [_o_] file dwim     [_A_] grep        [_v_] show all       [_p_] project
    [_r_] recent file   [_s_] occur       [_V_] ibuffer        [_i_] info
    [_d_] dir           [_S_] replace     [_K_] kill all
    [_l_] other         [_t_] find tag
    [_u_] test file     [_T_] make tags
    [_h_] root
                                                                        ╭────────┐
    Other Window      Run             Cache              Do             │ Fixmee │
  ╭──────────────────────────────────────────────────╯ ╭────────────────┴────────╯
    [_F_] file          [_U_] test        [_kc_] clear         [_x_] TODO & FIXME
    [_O_] dwim          [_m_] compile     [_kk_] add current   [_X_] toggle
    [_D_] dir           [_c_] shell       [_ks_] cleanup
    [_L_] other         [_C_] command     [_kd_] remove
    [_E_] buffer
  "
  ("<tab>" hydra-master/body "back")
  ("<ESC>" nil "quit")
  ("a"   projectile-ag)
  ("A"   projectile-grep)
  ("b"   projectile-switch-to-buffer)
  ("e"   helm-projectile-switch-to-buffer)
  ("E"   projectile-switch-to-buffer-other-window)
  ("c"   projectile-run-async-shell-command-in-root)
  ("C"   projectile-run-command-in-root)
  ("d"   helm-projectile-find-dir)
  ("D"   projectile-find-dir-other-window)
  ("f"   helm-projectile-find-file)
  ("F"   projectile-find-file-other-window)
  ("g"   projectile-vc)
  ("h"   projectile-dired)
  ("i"   projectile-project-info)
  ("kc"  projectile-invalidate-cache)
  ("kd"  projectile-remove-known-project)
  ("kk"  projectile-cache-current-file)
  ("K"   projectile-kill-buffers)
  ("ks"  projectile-cleanup-known-projects)
  ("o"   helm-projectile-find-file-dwim)
  ("O"   projectile-find-file-dwim-other-window)
  ("m"   projectile-compile-project)
  ("l"   helm-projectile-find-other-file)
  ("L"   projectile-find-other-file-other-window)
  ("p"   helm-projectile)
  ("r"   helm-projectile-recentf)
  ("s"   projectile-multi-occur)
  ("S"   projectile-replace)
  ("t"   projectile-find-tag)
  ("T"   projectile-regenerate-tags)
  ("u"   projectile-find-test-file)
  ("U"   projectile-test-project)
  ("v"   projectile-display-buffer)
  ("V"   projectile-ibuffer)
  ("X"   fixmee-mode)
  ("x"   fixmee-view-listing)
  ("q"   nil :color blue))




(defhydra hydra-navigate (:color pink :hint nil)
  "
^Align^              ^Sort^                    ^replace^           ^Search^
╭────────────────────────────────────────────────────────────────────────────────────────────────────────────────────╯
[_aa_] align         [_ol_] lines              [_e_] regexp        [_\'_] previous
[_ac_] current       [_op_] paragraphs         [_E_] string        [_,_]  next
[_ae_] entire        [_oP_] Pages        
[_ah_] highlight     [_of_] fields
[_an_] new           [_on_] numerics
[_ar_] regex         [_oc_] columns
[_au_] unhilight     [_or_] regex
 ^  ^                [_oR_] reverse
"
  ("n" forward-char)
  ("h" backward-char)
  ("r" forward-word)
  ("R" my-forward-block)
  ("g" backward-word)
  ("g" my-backward-block)
  ("l" my-select-current-line)
  ("L" my-select-current-block)
  ("t" next-line)
  ("c" previous-line)
  ("T" scroll-up-command)
  ("C" scroll-down-command)
  ("p" forward-paragraph)
  ("P" backward-paragraph)
  ("-" my-next-user-buffer)
  ("\\" my-previous-user-buffer)
  ("m" org-mark-ring-push)
  ("/" org-mark-ring-goto :color blue)
  ("b" beginning-of-buffer)
  ("B" end-of-buffer)
  ("d" beginning-of-line)
  ("D" end-of-line)
  ("[" backward-sexp)
  ("]" forward-sexp)

  ("aa" align)
  ("ac" align-current)
  ("ae" align-entire)
  ("ah" align-highlight-rule)
  ("an" align-newline-and-indent)
  ("ar" align-regexp)
  ("au" align-unhighlight-rule)

  ("ol" sort-lines)
  ("op" sort-paragraphs)
  ("oP" sort-pages)
  ("of" sort-fields)
  ("on" sort-numeric-fields)
  ("oc" sort-columns)
  ("or" sort-regexp-fields)
  ("oR" reverse-region)

  ("e" replace-regexp)
  ("E" replace-string)

  ("\'" smartscan-symbol-go-backward)
  ("," smartscan-symbol-go-forward)

  ("x" exchange-point-and-mark)
  ("w" delete-trailing-whitespace)
  ("s" er/expand-region)
  ("." hydra-repeat)
  ("q" nil :color blue)

  ("g" keyboard-quit))




(defhydra hydra-adjust (:color red :hint nil)
  "
      Zoom                Transparency
  ╭────────────────────────────────────────────╯
      [_s_] increase       [_t_] inscrese
      [_n_] decrease       [_h_] decrease
      [_l_] reset          [_r_] 100 [_c_] 30
"
  ("s" text-scale-increase)
  ("n" text-scale-decrease)
  ("l" text-scale-adjust 0)
  ("t" (transparency-increase))
  ("h" (transparency-decrease ))
  ("r" (transparency-set-value 100 ))
  ("c" (transparency-set-value 30 ))
  ("1" (transparency-set-value 10 ))
  ("2" (transparency-set-value 20 ))
  ("3" (transparency-set-value 30 ))
  ("4" (transparency-set-value 40 ))
  ("5" (transparency-set-value 50 ))
  ("6" (transparency-set-value 60 ))
  ("7" (transparency-set-value 70 ))
  ("8" (transparency-set-value 80 ))
  ("9" (transparency-set-value 90 ))
  ("q" nil :color blue)
  ("g" keyboard-quit))





(defhydra hydra-transpose (:color pink :hint nil)
  "
         Drag         Transpose                            Org
╭──────────────────────────────────────────────────────────────────────────╯
        ^_c_^         [_s_] characters  [_-_] sentences    [_o_] word
        ^^↑^^         [_w_] words       [_p_] paragraphs   [_e_] elements
    _h_ ←   → _n_     [_l_] line                         [_u_] table
        ^^↓^^        ╭─────────────────────┐
        ^_t_^         [_z_] cancel   [_y_ redo]
"
  ("c" drag-stuff-up)
  ("h" drag-stuff-left)
  ("n" drag-stuff-right)
  ("t" drag-stuff-down)

  ("s" transpose-chars)
  ("w" transpose-words)
  ("l" transpose-lines)
  ("-" transpose-sentences)
  ("p" transpose-paragraphs)
  ("o" org-transpose-words)
  ("e" org-transpose-elements)
  ("u" org-table-transpose-table-at-point)
  ("z" undo-tree-undo)
  ("y" undo-tree-redo)
  ("q" nil :color blue)
  ("g" keyboard-quit))


(defun ora-ex-point-mark ()
  (interactive)
  (if rectangle-mark-mode
      (exchange-point-and-mark)
    (let ((mk (mark)))
      (rectangle-mark-mode 1)
      (goto-char mk))))

(defhydra hydra-rectangle (:hint nil
                                 :body-pre (rectangle-mark-mode 1)
                                 :color pink
                                 :post (deactivate-mark))
  "
        ^_c_^
        ^^↑^^        [_e_] delete      [_j_] copy     [_r_] reset
    _h_ ←   → _n_    [_s_] tring       [_k_] paste    [_z_] undo
        ^^↓^^        [_x_] xchange     [_d_] kill     [_y_] redo
        ^_t_^
"
  ("h" backward-char nil)
  ("n" forward-char nil)
  ("c" previous-line nil)
  ("t" next-line nil)
  ("e" delete-rectangle nil)
  ("s" string-rectangle nil)
  ("x" ora-ex-point-mark nil)
  ("j" copy-rectangle-as-kill nil)
  ("k" yank-rectangle nil)
  ("d" kill-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("z" undo-tree-undo)
  ("y" undo-tree-redo)
  ("q" nil)
  ("g" keyboard-quit))





(defun my-mc/mark-previous-like-this (x)
  (interactive "p")
  (if (use-region-p)
      (mc/mark-previous-like-this 1)
    (progn
      (er/expand-region 1)
      (mc/mark-previous-like-this 1))))


(defun my-mc/mark-next-like-this (x)
  (interactive "p")
  (if (use-region-p)
      (mc/mark-next-like-this 1)
    (progn
      (er/expand-region 1)
      (mc/mark-next-like-this 1))))

(defun my-mc/quit-all () (progn () (mc/keyboard-quit) nil))


(defhydra hydra-multiple-cursors (:hint nil :color pink)
  "
     ^Up^            ^Down^          ^Multiple^    ^Other^       ^Search^
╭──────────────────────────────────────────────────────────────────────────────╯
    [_h_] Next      [_n_] Next      [_r_] Line    [_a_] All     [_._] Next
    [_H_] Skip      [_N_] Skip      [_l_] Begin   [_d_] Regex   [_,_] Previous
    [_c_] Unmark    [_t_] Unmark    [_/_] End     [_j_] Copy    [_k_] Paste
     ^ ^             ^ ^             ^ ^          [_e_] del
"
  ("h" my-mc/mark-previous-like-this)
  ("H" mc/skip-to-previous-like-this)
  ("t" mc/unmark-previous-like-this)
  ("n" my-mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("c" mc/unmark-next-like-this)
  ("r" mc/edit-lines)
  ("l" mc/edit-beginnings-of-lines)
  ("/" mc/edit-ends-of-lines)
  ("a" mc/mark-all-like-this)
  ("d" mc/mark-all-in-region-regexp)
  ("e" backward-delete-char-untabify)
  ("." phi-search)
  ("," phi-search-backward)
  ("j" copy-rectangle-as-kill)
  ("k" yank-rectangle)
  ("s" er/expand-region)
  ("z" undo-tree-undo)
  ("y" undo-tree-redo)
  ("q" nil :color blue)
  ("o" my-mc/quit-all :color blue)
  ("g" mc/keyboard-quit))




(defhydra hydra-major (:color teal :hint nil)
  "
    [_t_] text  [_d_] diff    [_l_] prog     [_o_] org
    [_h_] html  [_c_] css     [_s_] scss     [_j_] jinja
    [_J_] js    [_p_] python  [_C_] clojure  [_r_] ruby  [_e_] elisp
    [_n_] json  [_m_] md
"
  ("t" text-mode)
  ("d" diff-mode)
  ("l" prog-mode)
  ("o" org-mode)
  ("h" html-mode)
  ("c" css-mode)
  ("s" scss-mode)
  ("j" jinja2-mode)
  ("J" js2-mode)
  ("p" python-mode)
  ("C" clojure-mode)
  ("r" ruby-mode)
  ("e" emacs-lisp-mode)
  ("n" json-mode)
  ("m" markdown-mode)
  ("q" nil :color blue)
  ("g" keyboard-quit))



(defhydra hydra-minor (:color amaranth :hint nil)
  "
    [_a_] abbrev    [_d_] debug  [_f_] fill  [_l_] line    [_n_] nyan
    [_r_] truncate  [_s_] save   [_t_] typo  [_v_] visual  [_w_] white
    [_e_] desktop
"
  ("a" abbrev-mode)
  ("d" toggle-debug-on-error)
  ("f" auto-fill-mode)
  ("l" line-number-mode)
  ("n" nyan-mode)
  ("r" toggle-truncate-lines)
  ("s" auto-save-buffers-enhanced-toggle-activity)
  ("t" typo-mode)
  ("w" whitespace-mode)
  ("v" visual-line-mode)
  ("e" desktop-save-mode)
  ("q" nil :color blue)
  ("g" nil))



(defhydra hydra-apropos (:color teal :hint nil)
  "Apropos"
  ("a" helm-apropos "apropos")
  ("c" apropos-command "cmd")
  ("d" apropos-documentation "doc")
  ("e" apropos-value "val")
  ("l" apropos-library "lib")
  ("o" apropos-user-option "option")
  ("u" apropos-user-option "option")
  ("v" apropos-variable "var")
  ("i" info-apropos "info")
  ("t" tags-apropos "tags")
  ("z" hydra-customize-apropos/body "customize")
  ("q" nil :color blue)
  ("g" keyboard-quit))



(defhydra hydra-desktop (:color blue)
  ("c" desktop-clear "clear")
  ("s" desktop-save "save")
  ("r" desktop-revert "revert")
  ("d" desktop-change-dir "dir")
  ("t" desktop-save-mode "toggle mode"))


(defhydra hydra-theme (:color red)
  ("g" (load-theme 'grandshell) "grandshell")
  ("t" (load-theme 'gotham) "gotham")
  ("l" (load-theme 'leuven) "leuven")
  ("m" (load-theme 'monokai) "monokai")
  ("n" (load-theme 'noctilux) "noctilux")
  ("q" nil :color blue))




(defhydra hydra-helm (:color teal :hint nil)
  "
[_x_]  M-x    [_y_] ring      [_b_] mini     [_f_] find     [_s_] grep
[_i_] imenu   [_hm_] find     [_h/_] locate  [_l_] occur    [_a_] apropos
[_hhg_] gnus  [_hhi_] info    [_hhr_] emacs  [_cr_] resume  [_m_] mark
[_r_] regex   [_p_] register  [_t_] top      [_cs_] surf    [_g_] google
[_cc_] color  [_:_] eldoc     [_,_] calcul   [_ci_] input   [_cm_] hist
"
  ("x" helm-M-x)
  ("y" helm-show-kill-ring)
  ("b" helm-mini)
  ("f" helm-find-files)
  ("s" helm-ff-run-grep)
  ("i" helm-semantic-or-imenu)
  ("hm" helm-find)
  ("h/" helm-locate)
  ("l" helm-occur)
  ("a" helm-apropos)
  ("hhg" helm-info-gnus)
  ("hhi" helm-info-at-point)
  ("hhr" helm-info-emacs)
  ("cr" helm-resume)
  ("m" helm-all-mark-rings)
  ("r" helm-regexp)
  ("p" helm-register)
  ("t" helm-top)
  ("cs" helm-surfraw)
  ("g" helm-google-suggest)
  ("cc" helm-colors)
  (":" helm-eval-expression-with-eldoc)
  ("," helm-calcul-expression)
  ("ci" helm-comint-input-ring)
  ("cm" helm-minibuffer-history)
  ("g" keyboard-quit)
  ("q" nil :color blue))


(defhydra hydra-outline (:color pink :hint nil)
  "
    ^Hide^             ^Show^           ^Move^
  ╭──────────────────────────────────────────────────────────────────╯
    [_q_] sublevels     [_a_] all         [_u_] up
    [_t_] body          [_e_] entry       [_c_] previous visible
    [_o_] other         [_i_] children    [_t_] next visible
    [_r_] entry         [_k_] branches    [_n_] forward same level
    [_l_] leaves        [_s_] subtree     [_h_] backward same level
    [_d_] subtree
"
  ("q" hide-sublevels)    ; Hide everything but the top-level headings
  ("t" hide-body)         ; Hide everything but headings (all body lines)
  ("o" hide-other)        ; Hide other branches
  ("r" hide-entry)        ; Hide this entry's body
  ("l" hide-leaves)       ; Hide body lines in this entry and sub-entries
  ("d" hide-subtree)      ; Hide everything in this entry and sub-entries
  ("a" show-all)          ; Show (expand) everything
  ("e" show-entry)        ; Show this heading's body
  ("i" show-children)     ; Show this heading's immediate child sub-headings
  ("k" show-branches)     ; Show all sub-headings under this heading
  ("s" show-subtree)      ; Show (expand) everything in this heading & below
  ("u" outline-up-heading)                ; Up
  ("c" outline-previous-visible-heading)      ; Next
  ("t" outline-next-visible-heading)  ; Previous
  ("n" outline-forward-same-level)        ; Forward - same level
  ("h" outline-backward-same-level)       ; Backward - same level
  ("z" undo-tree-undo)
  ("y" undo-tree-redo)
  ("g" keyboard-quit)
  ("q" nil :color blue))




(defhydra hydra-markdown (:color pink :hint nil)
  "
Formatting        C-c C-s    _s_: bold          _e_: italic     _b_: blockquote   _p_: pre-formatted    _u_: code
Headings          C-c C-t    _h_: automatic     _1_: h1         _2_: h2           _3_: h3               _4_: h4
Lists             C-c C-x    _m_: insert item
Demote/Promote    C-c C-x    _l_: promote       _r_: demote     _c_: move up      _t_: move down
Links, footnotes  C-c C-a    _L_: link          _U_: uri        _F_: footnote     _W_: wiki-link      _R_: reference
"
  ("s" markdown-insert-bold)
  ("e" markdown-insert-italic)
  ("b" markdown-insert-blockquote :color blue)
  ("p" markdown-insert-pre :color blue)
  ("u" markdown-insert-code)
  ("h" markdown-insert-header-dwim)
  ("1" markdown-insert-header-atx-1)
  ("2" markdown-insert-header-atx-2)
  ("3" markdown-insert-header-atx-3)
  ("4" markdown-insert-header-atx-4)
  ("m" markdown-insert-list-item)
  ("l" markdown-promote)
  ("r" markdown-demote)
  ("c" markdown-move-down)
  ("t" markdown-move-up)
  ("L" markdown-insert-link :color blue)
  ("U" markdown-insert-uri :color blue)
  ("F" markdown-insert-footnote :color blue)
  ("W" markdown-insert-wiki-link :color blue)
  ("R" markdown-insert-reference-link-dwim :color blue)
  ("z" undo-tree-undo)
  ("y" undo-tree-redo)
  ("g" keyboard-quit)
  ("q" nil :color blue))




(bind-key "C-x o" 'hydra-window/body)
(bind-key "C-x e" 'hydra-elscreen/body)
(bind-key "C-t" 'hydra-multiple-cursors/body)
(bind-key "C-s" 'hydra-navigate/body)

(bind-key "C-x -" 'hydra-yasnippet/body)
(bind-key "C-x p" 'hydra-project/body)
(bind-key "C-x =" 'hydra-adjust/body)
(bind-key "C-x t" 'hydra-transpose/body)
(bind-key "C-x r" 'hydra-rectangle/body)
(bind-key "C-x a" 'hydra-apropos/body)
(bind-key "C-x M" 'hydra-major/body)
(bind-key "C-x m" 'hydra-minor/body)
(bind-key "C-x v" 'hydra-theme/body)
(bind-key "C-x h" 'hydra-helm/body)
(bind-key "C-x d" 'hydra-desktop/body)

(add-hook 'outline-mode-hook (lambda() (bind-key "C-x x" #'hydra-outline/body outline-mode-map)))
(add-hook 'markdown-mode-hook (lambda() (bind-key "C-x x" #'hydra-markdown/body markdown-mode-map)))





;; ;; REGION KEYBINDING
;; (use-package region-bindings-mode
;;   :ensure t
;;   :diminish region-bindings-mode
;;   :config(progn
;;            (region-bindings-mode-enable)
;;            (bind-key "h" #'mc/mark-previous-like-this region-bindings-mode-map)
;;            (bind-key "n" #'mc/mark-next-like-this region-bindings-mode-map)
;;            (bind-key "H" #'mc/skip-to-previous-like-this region-bindings-mode-map)
;;            (bind-key "N" #'mc/skip-to-next-like-this region-bindings-mode-map)
;;            (bind-key "u" #'universal-argument region-bindings-mode-map)
;;            (bind-key "c" #'mc/unmark-next-like-this region-bindings-mode-map)
;;            (bind-key "t" #'mc/unmark-previous-like-this region-bindings-mode-map)
;;            (bind-key "a" #'mc/mark-all-like-this region-bindings-mode-map)
;;            (bind-key "m" #'mc/mark-more-like-this-extended region-bindings-mode-map)
;;            (bind-key "r" #'mc/edit-lines region-bindings-mode-map)
;;            (bind-key "l" #'mc/edit-beginnings-of-lines region-bindings-mode-map)
;;            (bind-key "/" #'mc/edit-ends-of-lines region-bindings-mode-map)
;;            (bind-key "f" #'mc/mark-all-in-region-regexp region-bindings-mode-map)
;;            (bind-key "." #'phi-search region-bindings-mode-map)
;;            (bind-key "," #'phi-search-backward region-bindings-mode-map)
;;            (bind-key "e" #'backward-delete-char region-bindings-mode-map)
;;            (bind-key "g" #'keyboard-escape-quit region-bindings-mode-map)
;;            (bind-key "C-g" #'keyboard-escape-quit region-bindings-mode-map)
;;            (bind-key "-" #'comment-dwim region-bindings-mode-map)
;;            (bind-key "q" #'kill-region region-bindings-mode-map)
;;            (bind-key "j" #'kill-ring-save region-bindings-mode-map)
;;            (bind-key "k" #'yank region-bindings-mode-map)
;;            (bind-key "x" #'kill-rectangle region-bindings-mode-map)
;;            (bind-key "b" #'replace-rectangle region-bindings-mode-map)
;;            (bind-key "d" #'duplicate-current-line-or-region region-bindings-mode-map)
;;            (bind-key "j" #'join-line-or-lines-in-region region-bindings-mode-map)
;;            (bind-key "9" #'sp-splice-sexp region-bindings-mode-map)
;;            (bind-key "o" #'exchange-point-and-mark region-bindings-mode-map)
;;            (bind-key "y" #'search-forward-regexp region-bindings-mode-map)
;;            (bind-key "p" #'replace-regexp region-bindings-mode-map)
;;            (bind-key "M-o" #'exchange-point-and-mark region-bindings-mode-map)))


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
    ("0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "196cc00960232cfc7e74f4e95a94a5977cb16fd28ba7282195338f68c84058ec" "dcf229d4673483cb7b38505360824fa56a0d7b52f54edbcdca98cf5059fa1662" "067d9b8104c0a98c916d524b47045367bdcd9cf6cda393c5dae8cd8f7eb18e2a" "0820d191ae80dcadc1802b3499f84c07a09803f2cb90b343678bdb03d225b26b" "94ba29363bfb7e06105f68d72b268f85981f7fba2ddef89331660033101eb5e5" "cdd26fa6a8c6706c9009db659d2dffd7f4b0350f9cc94e5df657fa295fffec71" "47ac4658d9e085ace37e7d967ea1c7d5f3dfeb2f720e5dec420034118ba84e17" "af960831c1b33b719cda2ace858641dd8accc14d51e8ffb65b39ca75f07d595d" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "8fed5e4b89cf69107d524c4b91b4a4c35bcf1b3563d5f306608f0c48f580fdf8" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "3ed645b3c08080a43a2a15e5768b893c27f6a02ca3282576e3bc09f3d9fa3aaa" "f0d8af755039aa25cd0792ace9002ba885fd14ac8e8807388ab00ec84c9497d7" "11636897679ca534f0dec6f5e3cb12f28bf217a527755f6b9e744bd240ed47e1" "50ce37723ff2abc0b0b05741864ae9bd22c17cdb469cae134973ad46c7e48044" "08851585c86abcf44bb1232bced2ae13bc9f6323aeda71adfa3791d6e7fea2b6" "01d299b1b3f88e8b83e975484177f89d47b6b3763dfa3297dc44005cd1c9a3bc" "c3c0a3702e1d6c0373a0f6a557788dfd49ec9e66e753fb24493579859c8e95ab")))
 '(delete-selection-mode 1)
 '(exec-path
   (append exec-path
           (quote
            ("/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/sbin" "/bin" "/opt/node/bin"))))
 '(js2-highlight-level 3)
 '(package-selected-packages
   (quote
    (unicode-fonts buffer-move neotree cider-mode cider popwin elisp--witness--lisp company-irony expand-region company-quickhelp company yaml-mode windata use-package tree-mode smartparens shm scss-mode rainbow-delimiters python-info pydoc-info php-mode nyan-mode multiple-cursors molokai-theme markdown-mode lua-mode leuven-theme json-rpc json-mode js2-mode jinja2-mode jedi iedit hi2 helm-swoop helm-projectile helm-hoogle helm-ghc helm-css-scss helm-company goto-chg fullscreen-mode framemove f emmet-mode drag-stuff company-tern company-jedi company-ghc coffee-mode auto-save-buffers-enhanced auto-compile)))
 '(prefer-coding-system (quote utf-8))
 '(ring-bell-function (quote ignore) t)
 '(same-window-buffer-names (quote ("*shell*")))
 '(scroll-error-top-bottom t)
 '(sp-highlight-pair-overlay nil)
 '(sp-highlight-wrap-overlay nil)
 '(sp-highlight-wrap-tag-overlay nil)
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
(load-theme 'grandshell)
;; (load-theme 'molokai)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-error ((t nil)))
 '(js2-warning ((t nil)))
 '(js2-warnings ((t nil)))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "royal blue"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "firebrick"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "forest green"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "dark magenta"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "gold3"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "DarkOrange3"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "magenta")))))
