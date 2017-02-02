(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-verbose t)
(require 'use-package)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(unbind-key "<tab>" minibuffer-local-completion-map)
(unbind-key "<tab>" minibuffer-local-map)


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
                ;; helm-mode-handle-completion-in-region t
                helm-persistent-help-string nil
                helm-boring-buffer-regexp-list
                (quote
                 ("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf" "\\*.*\\*" "\\*magit")))
          (setq helm-c-source-swoop-match-functions
                '(helm-mm-exact-match
                  helm-mm-match
                  ;;helm-fuzzy-match
                  ;;helm-mm-3-migemo-match
                  ))
          (defadvice helm-display-mode-line (after undisplay-header activate)
            (setq header-line-format nil ))
          (helm-mode))
  :config
  (progn
    (bind-key "<tab>" #'helm-execute-persistent-action helm-map) ; rebind tab to do persistent action
    (bind-key "C-i" #'helm-execute-persistent-action helm-map) ; make TAB works in terminal
    (bind-key "C-z" #'helm-select-action helm-map) ; list actions using C-z
    (bind-key "M-c" #'helm-previous-line helm-map)
    (bind-key "M-t" #'helm-next-line helm-map)
    (bind-key "M-o" #'helm-next-source helm-map)
    (bind-key "M-C" #'helm-previous-page helm-map)
    (bind-key "M-T" #'helm-next-page helm-map)
    (bind-key "M-b" #'helm-beginning-of-buffer helm-map)
    (bind-key "M-B" #'helm-end-of-buffer helm-map)
    (bind-key "C-h" #'helm-find-files-up-one-level helm-find-files-map)
    (bind-key "C-n" #'helm-find-files-down-last-level helm-find-files-map)
    (bind-key "M-C" #'helm-previous-page helm-find-files-map)
    (bind-key "M-B" #'helm-end-of-buffer helm-find-files-map)
    (bind-key "C-f" #'helm-ff-run-find-sh-command helm-find-files-map)
    (bind-key "C-S-f" #'helm-ff-run-locate helm-find-files-map)
    (bind-key "C-e" #'helm-ff-run-eshell-command-on-file helm-find-files-map)
    (bind-key "C-r" #'helm-ff-run-rename-file helm-find-files-map) 
    (bind-key "C-j" #'helm-ff-run-copy-file helm-find-files-map) 
    (bind-key "C-d" #'helm-ff-run-delete-file helm-find-files-map) 
    (bind-key "C-s" #'helm-ff-run-grep helm-find-files-map) 
    (bind-key "C-S-d" #'helm-buffer-run-kill-persistent helm-buffer-map)
    (bind-key "C-d" #'helm-buffer-run-kill-buffers helm-buffer-map)
    (bind-key "M-SPC" #'helm-toggle-visible-mark helm-map)))


(use-package helm-swoop
  :ensure t
  :init (progn
          (setq helm-c-source-swoop-search-functions
                '(helm-mm-exact-search
                  helm-mm-search
                  helm-candidates-in-buffer-search-default-fn)
                helm-swoop-pre-input-function (lambda () "")))
  :config (progn
            (bind-key "C-c C-t" 'toggle-case-fold-search helm-swoop-map)
            (bind-key "C-c C-t" 'toggle-case-fold-search helm-swoop-edit-map)
            ;; (bind-key "C-c C-c" 'helm-swoop--edit-complete helm-swoop-edit-map)
            ;; (bind-key "C-c C-c" 'helm-swoop--edit-complete helm-multi-swoop-edit-map)
            ))

(use-package helm-ag
  :ensure t :defer t
  :init (progn
          (setq helm-ag-insert-at-point 'symbol))
  :config(progn
           (bind-key  "C-c C-e" 'helm-ag-edit helm-map)))

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
                company-tooltip-minimum-width 40
                company-idle-delay 0.1
                company-echo-delay 0
                company-show-numbers t
                company-minimum-prefix-length 1
                company-quickhelp-delay nil)
          (company-quickhelp-mode 1))

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
            (bind-key "C-t" #'company-quickhelp-manual-begin company-active-map)
            (unbind-key "M-h" company-active-map)
            (unbind-key "M-n" company-active-map)
            (bind-key "C-i" #'yas-expand company-active-map)))



(use-package company-quickhelp
  :ensure t :defer t)

(use-package ycmd
  :ensure t :defer t
  :init (progn
          (set-variable 'ycmd-server-command '("python" "/opt/ycmd/ycmd"))))

(use-package company-ycmd
  :ensure t :defer t
  :init (progn
          (company-ycmd-setup)))

(use-package eldoc
  :ensure t :defer t
  :config (progn
            (add-hook 'prog-mode-hook 'eldoc-mode)))

(use-package magit
  :ensure t :defer t)

(use-package helm-projectile
  :ensure t :defer t)

(use-package ag
  :ensure t :defer t)

(use-package projectile
  :ensure t :defer t :diminish projectile-mode
  :init(progn
         (setq projectile-enable-caching t
               projectile-completion-system 'helm
               projectile-switch-project-action 'helm-projectile-find-file
               ;; projectile-use-native-indexing t)
               projectile-use-native-indexing nil
               projectile-mode-line "Projectile")
         (helm-projectile-on)
         (projectile-global-mode)))


(use-package yasnippet
  :ensure t
  :diminish yas-minor-1mode
  :init(progn
         (yas-global-mode 1))
  :config(progn
           (setq yas-installed-snippets-dir "~/.emacs.d/snippets")
           (define-key yas-minor-mode-map (kbd "TAB") nil)
           (define-key yas-minor-mode-map (kbd "<tab>") nil)
           (bind-key "M--" #'yas-expand yas-minor-mode-map)))

(use-package yafolding :ensure t)


(use-package hydra
  :ensure t :defer t)

(use-package exec-path-from-shell
  :ensure t
  :init (progn
          (exec-path-from-shell-initialize)))


(use-package edit-server
  :ensure t :defer t
  :init (progn
          (setq edit-server-new-frame nil)))

(use-package tramp
  :defer t
  :init (progn
          (setq tramp-default-method "ssh"
                password-cache-expiry nil)))

(use-package simple-httpd
  :ensure t :defer t)

(use-package alpha
  :ensure t)

(use-package quickrun
  :ensure t :defer t
  :init (progn
          (setq quickrun-focus-p nil)))

(use-package shell
  :defer t
  :init(progn
         (defun comint-clear-buffer ()
           (interactive)
           (let ((comint-buffer-maximum-size 0))
             (comint-truncate-buffer)))
         (bind-key "<up>" 'comint-previous-input shell-mode-map)
         (bind-key "<down>" 'comint-next-input shell-mode-map)
         (bind-key "C-r" 'comint-history-isearch-backward-regexp shell-mode-map)
         (bind-key "C-S-r" 'helm-swoop shell-mode-map)
         (bind-key "C-p" 'helm-comint-input-ring shell-mode-map)
         (bind-key "C-y" 'helm-comint-input-ring shell-mode-map)
         (bind-key "C-l" 'comint-clear-buffer shell-mode-map)))

(use-package eshell
  :init (progn
          (defun eshell/clear ()
            (let ((inhibit-read-only t))
              (erase-buffer)
              (eshell-send-input)))
          (add-hook 'eshell-mode-hook (lambda ()
                                        (bind-key "M-d" 'eshell-bol eshell-mode-map)
                                        (bind-key "M-q" 'eshell-kill-input eshell-mode-map)
                                        (bind-key "M-H" 'eshell-previous-prompt eshell-mode-map)
                                        (bind-key "M-N" 'eshell-next-prompt eshell-mode-map)
                                        (bind-key "C-l" 'eshell/clear eshell-mode-map)
                                        (bind-key "<up>" 'eshell-previous-input eshell-mode-map)
                                        (bind-key "<down>" 'eshell-next-input eshell-mode-map)
                                        (bind-key "<tab>" 'completion-at-point eshell-mode-map)
                                        (bind-key "TAB" 'completion-at-point eshell-mode-map)))))

(use-package comment-dwim-2
  :ensure t :defer t)


(use-package smartparens
  :ensure t :defer t :diminish smartparens-mode
  :init (progn
          (smartparens-global-mode t)
          (show-smartparens-global-mode t)
          (setq sp-highlight-pair-overlay nil
                sp-highlight-wrap-overlay nil
                sp-highlight-wrap-tag-overlay nil))
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

(use-package bookmark+
  :ensure t :defer t)

(use-package elscreen
  :ensure t :defer t
  :init (progn
          (setq elscreen-display-screen-number t
                elscreen-display-tab nil
                elscreen-default-buffer-initial-major-mode (quote lisp-interaction-mode)
                elscreen-default-buffer-initial-message nil)
          (elscreen-start)))


(use-package framemove
  :ensure t :defer t)

(use-package buffer-move
  :ensure t)

(use-package winner-mode
  :defer t
  :init (progn
          (winner-mode)))

(use-package popwin
  :ensure t
  :config(progn
           (popwin-mode 1)))

(use-package smartscan
  :ensure t)

(use-package auto-save-buffers-enhanced
  :ensure t
  :init(progn
         (auto-save-buffers-enhanced nil)))

(use-package recentf-mode
  :defer t :diminish recentf-mode
  :init(progn
         (recentf-mode t)))

(use-package drag-stuff
  :diminish drag-stuff-mode
  :ensure t)

(use-package expand-region
  :ensure t :defer t)

(use-package goto-chg
  :ensure t :defer t)

(use-package window-number
  :ensure t)

(use-package neotree
  :ensure t :defer t
  :init (progn
          (setq neo-window-fixed-size t
                neo-window-width 30))
  :config(progn
           (defun neotree-enter-in-place ()
             (interactive)
             (neotree-enter)
             (neotree-show))
           (defun neotree-enter-quit ()
             (interactive)
             (neotree-enter)
             (neotree-hide))
           (bind-key "<tab>" 'neotree-enter neotree-mode-map)
           (bind-key "RET" 'neotree-enter-quit neotree-mode-map)
           (bind-key "e" 'neotree-enter neotree-mode-map)
           (bind-key "o" 'neotree-enter-in-place neotree-mode-map)
           (bind-key "r" 'neotree-rename-node neotree-mode-map)
           (bind-key "d" 'neotree-delete-node neotree-mode-map)
           (bind-key "a" 'neotree-create-node neotree-mode-map)
           (bind-key "." 'neotree-hidden-file-toggle neotree-mode-map)
           (bind-key "m" 'neotree-dir neotree-mode-map)
           (bind-key "h" 'neotree-select-previous-sibling-node neotree-mode-map)
           (bind-key "n" 'neotree-select-next-sibling-node neotree-mode-map)
           (bind-key "c" 'neotree-previous-line neotree-mode-map)
           (bind-key "t" 'neotree-next-line neotree-mode-map)
           (bind-key "'" 'neotree-enter-horizontal-split neotree-mode-map)
           (bind-key "," 'neotree-enter-vertical-split neotree-mode-map)
           (bind-key "j" 'neotree-copy-node neotree-mode-map)
           (bind-key "u" 'neotree-select-up-node neotree-mode-map)
           (bind-key "i" 'neotree-select-down-node neotree-mode-map)
           (bind-key "s" 'neotree-change-root neotree-mode-map)
           (bind-key "1" 'neotree-window-1 neotree-mode-map)))

(use-package beacon
  :ensure t :defer t
  :diminish beacon-mode
  :init (progn
          (beacon-mode)))

(use-package nyan-mode
  :ensure t :defer t)


(use-package visual-regexp
  :ensure t
  :config (progn
          (bind-key "C-c ." 'hide-lines-show-all  vr/minibuffer-keymap )))


(use-package multiple-cursors
  :ensure t
  :diminish multiple-cursors-mode
  :config (progn
          (bind-key "C--" 'mc-hide-unmatched-lines-mode mc/keymap)))

(use-package phi-search :ensure t)


(use-package dired+
  :ensure t
  :init (progn
          (defun my-dired-create-file (file)
            (interactive
             (list (read-file-name "Create file: " (dired-current-directory))))
            (let* ((expanded (expand-file-name file))
                   (try expanded)
                   (dir (directory-file-name (file-name-directory expanded)))
                   new)
              (if (file-exists-p expanded)
                  (error "Cannot create file %s: file exists" expanded))
              ;; Find the topmost nonexistent parent dir (variable `new')
              (while (and try (not (file-exists-p try)) (not (equal new try)))
                (setq new try
                      try (directory-file-name (file-name-directory try))))
              (when (not (file-exists-p dir))
                (make-directory dir t))
              (write-region "" nil expanded t)
              (when new
                (dired-add-file new)
                (dired-move-to-filename))
              (revert-buffer)))
          
          (add-hook 'dired-mode-hook 'auto-revert-mode)
          (diredp-toggle-find-file-reuse-dir t))
  :config (progn
            (bind-key "a" 'dired-toggle-marks dired-mode-map)
            (bind-key "t" 'my-dired-create-file dired-mode-map)
            (bind-key "M-c" 'diredp-previous-line dired-mode-map)
            (bind-key "M-C" 'scroll-down-command dired-mode-map)
            (bind-key "M-T" 'scroll-up-command dired-mode-map)
            (bind-key "M-b" 'beginning-of-buffer dired-mode-map)
            (bind-key "M-B" 'end-of-buffer dired-mode-map)
            (bind-key "l" 'dired-hide-details-mode dired-mode-map)
            (bind-key "<tab>" 'dired-subtree-toggle dired-mode-map)
            (bind-key "<backtab>" 'dired-subtree-cycle dired-mode-map)
            (bind-key "M-G" 'dired-subtree-beginning dired-mode-map)
            (bind-key "M-R" 'dired-subtree-end dired-mode-map)
            (bind-key "1" 'dired-ranger-copy dired-mode-map)
            (bind-key "2" 'dired-ranger-paste dired-mode-map)
            (bind-key "3" 'dired-ranger-move dired-mode-map)
            (bind-key "4" 'dired-do-delete dired-mode-map)
            (bind-key "C-w" 'kill-this-buffer dired-mode-map)
            (define-key dired-mode-map (kbd ".") dired-filter-map)))
  

(use-package dired-subtree :ensure t :defer t)
(use-package dired-filter :ensure t :defer t)
(use-package dired-ranger :ensure t :defer t)



(use-package emmet-mode
  :ensure t
  :init(progn
         (add-hook 'web-mode-hook 'emmet-mode)
         (add-hook 'html-mode-hook 'emmet-mode)
         (add-hook 'jinja2-mode-hook 'emmet-mode)
         (add-hook 'css-mode-hook 'emmet-mode)
         (setq emmet-indentation 2)
         (setq emmet-preview-default nil)
         (add-hook' emmet-mode-hook (lambda()
                                      (bind-key "C-c C-w" #'emmet-wrap-with-markup emmet-mode-keymap)
                                      (bind-key "C-c w" #'emmet-wrap-with-markup emmet-mode-keymap)))))


;; WEB BEAUTY
(use-package web-beautify
  :ensure t :defer t)

(use-package web-mode
  :ensure t :defer t
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.ejs?\\'" . web-mode))))


(use-package sws-mode :ensure t :defer t)
(use-package jade-mode
  :ensure t :defer t
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))
          (add-to-list 'auto-mode-alist '("\\.pug\\'" . jade-mode))))

(use-package stylus-mode
  :ensure t :defer t
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.styl\\'" . jade-mode))))

(use-package jinja2-mode
  :ensure t :defer t)

(use-package handlebars-mode
  :ensure t :defer t
  :init(progn
         (add-to-list 'auto-mode-alist '("\\.hbs\\'" . handlebars-mode))))



(use-package css-mode
  :defer t
  :config(progn
           (bind-key "C-p" 'helm-css-scss css-mode-map)
           (bind-key "C-c C-b" 'web-beautify-css css-mode-map)))



(use-package scss-mode
  :ensure t :defer t
  :init(progn
         (setq scss-compile-at-save nil))
  :config(progn
           (bind-key "C-p" 'helm-css-scss scss-mode-map)))

(use-package markdown-mode
  :ensure t :defer t
  :init(progn
         (setq markdown-xhtml-standalone-regexp "")))


(use-package json-mode
  :ensure t :defer t
  :init(progn
         (add-hook 'json-mode-hook (lambda ()
                                     (make-local-variable 'js-indent-level)
                                     (setq js-indent-level 2)
                                     (flycheck-mode -1)
                                     (tern-mode -1)))))

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


(use-package ggtags
  :ensure t :defer t
  :config (progn
          (unbind-key "M-n" ggtags-navigation-mode-map )
          (unbind-key "M-p" ggtags-navigation-mode-map )))

(use-package flycheck
  :ensure t :defer t
  :init (progn
          (setq flycheck-check-syntax-automatically '(mode-enabled save))))

(use-package undo-tree
  :diminish undo-tree-mode
  :ensure t :defer t
  :init (progn
          (global-undo-tree-mode)
          (setq undo-tree-visualizer-timestamps t)))



;; THEMES
(use-package grandshell-theme :ensure t :defer t)
(use-package monokai-theme :ensure t :defer t)
(use-package cyberpunk-theme :ensure t :defer t)
(use-package assemblage-theme :ensure t :defer t)
(use-package color-theme-sanityinc-tomorrow :ensure t :defer t)


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
                                 (mode . js2-jsx-mode)
                                 (mode . rust-mode)
                                 (mode . go-mode)
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


(use-package doc-view
  :defer t
  :init (progn
          (with-eval-after-load 'doc-view
            (bind-key "h" 'doc-view-previous-page doc-view-mode-map )
            (bind-key "n" 'doc-view-next-page doc-view-mode-map )
            (bind-key "c" 'previous-line doc-view-mode-map )
            (bind-key "t" 'next-line doc-view-mode-map )
            (bind-key "g" 'scroll-down-command doc-view-mode-map )
            (bind-key "r" 'scroll-up-command doc-view-mode-map )
            (bind-key "b" 'doc-view-first-page doc-view-mode-map )
            (bind-key "B" 'doc-view-last-page doc-view-mode-map )
            (bind-key "l" 'doc-view-goto-page doc-view-mode-map )
            (bind-key "/" 'doc-view-shrink doc-view-mode-map )
            (bind-key "=" 'doc-view-enlarge doc-view-mode-map ))))


(use-package org-mime
  :ensure t :defer t)

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
                  (dot . t)
                  (plantuml . t)
                  (sh . t)
                  (python . t)
                  (emacs-lisp . t)))
                org-confirm-babel-evaluate nil
                org-plantuml-jar-path  (expand-file-name "~/.emacs.d/elpa/org-20160118/scripts/plantuml.jar")
                org-babel-python-command "python3"))
  :config (progn
            (add-hook 'org-mode-hook (lambda () (visual-line-mode)))
            (unbind-key "C-e" org-mode-map)
            (unbind-key "C-j" org-mode-map)))

(use-package ox-pandoc
  :ensure t :defer t)


(use-package ob-mongo
  :ensure t :defer t)


;; PYTHON
(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :init (progn
          (setq expand-region-preferred-python-mode (quote fgallina-python)
                python-shell-interpreter "python3"
                python-shell-interpreter-args "--simple-prompt -i"))
  :config (progn
            (bind-key "C-c C-r" 'python-shell-send-region python-mode-map)
            (bind-key "C-c C-c" 'python-shell-send-defun python-mode-map)
            (bind-key "C-c C-k" 'python-shell-send-buffer python-mode-map)))


(use-package elpy
  :ensure t
  :init(progn
         (defun my/python-refactoring ()
           (interactive)
           (save-buffer)
           (elpy-refactor))
         
         (add-hook 'python-mode-hook
                   (lambda ()
                     (elpy-enable)
                     (flycheck-mode 1)))
         
         (setq elpy-rpc-python-command "python3"
               elpy-rpc-backend "jedi"
               elpy-syntax-check-command "flake8"
               elpy-modules (quote(elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-sane-defaults))))
         
  :config(progn
           (bind-key "C-c C-f" 'elpy-format-code elpy-mode-map)
           (bind-key "C-c f" 'elpy-format-code elpy-mode-map)
           (bind-key "C-c C-," 'elpy-goto-definition elpy-mode-map)
           (bind-key "C-c ," 'elpy-goto-definition elpy-mode-map)
           (bind-key "C-c C-'" 'pop-tag-mark elpy-mode-map)
           (bind-key "C-c '" 'pop-tag-mark elpy-mode-map)
           (bind-key "C-c C-o" 'elpy-goto-definition-other-window elpy-mode-map)
           (bind-key "C-c o" 'elpy-goto-definition-other-window elpy-mode-map)
           (bind-key "C-c C-i" 'elpy-importmagic-add-import elpy-mode-map)
           (bind-key "C-c i " 'elpy-importmagic-add-import elpy-mode-map)
           (bind-key "C-c C-m" 'elpy-importmagic-fixup elpy-mode-map)
           (bind-key "C-c m" 'elpy-importmagic-fixup elpy-mode-map)
           (bind-key "C-c C-t" 'elpy-test elpy-mode-map)
           (bind-key "C-c t" 'elpy-test elpy-mode-map)
           (bind-key "C-c C-s" 'elpy-rgrep-symbol elpy-mode-map)
           (bind-key "C-c r" 'my/python-refactoring elpy-mode-map)
           (bind-key "C-c C-r" 'my/python-refactoring elpy-mode-map)
           (bind-key "C-c C-c" 'elpy-shell-send-current-statement elpy-mode-map)
           (bind-key "C-x C-e" 'elpy-shell-send-current-statement elpy-mode-map)
           
           (unbind-key "C-c C-r" elpy-mode-map)
           (unbind-key "C-c C-l" elpy-mode-map)
           (unbind-key "C-c C-k" elpy-mode-map)
           (unbind-key "C-c C-c" elpy-mode-map)))

(use-package jedi
  :ensure t :defer t)

(use-package pyvenv
  :ensure t)




;; RUBY
(use-package seeing-is-believing :ensure t)
(use-package inf-ruby :ensure t :defer t)
(use-package company-inf-ruby :ensure t :defer t)


(use-package ruby-mode
  :defer t
  :init (progn
          (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
          (add-hook 'ruby-mode-hook
                    (lambda ()
                      (flycheck-mode 1)
                      (set (make-local-variable 'company-backends) '((company-inf-ruby company-dabbrev-code ))))))
  :config(progn
           (bind-key "<f8>" 'inf-ruby ruby-mode-map)
           (bind-key "C-c C-c" 'ruby-send-block ruby-mode-map)
           (bind-key "C-c C-b" 'ruby-send-last-sexp ruby-mode-map)
           (bind-key "C-c C-k" 'ruby-send-buffer ruby-mode-map)
           (bind-key "C-c C-z" 'run-ruby ruby-mode-map)))



;; JAVASCRIPT
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-jsx-mode)
  :init (progn
          (defun my/js2-toggle-indent ()
            (interactive)
            (setq js-indent-level (if (= js-indent-level 2) 4 2))
            (setq js2-indent-level (if (= js-indent-level 2) 4 2))
            (setq js2-basic-offset (if (= js-indent-level 2) 4 2))
            (message "js-indent-level, js2-indent-level, and js2-basic-offset set to %d"
                     js2-basic-offset))
          (setq js2-basic-indent 2
                js2-basic-offset 2
                js2-highlight-level 2
                js2-auto-indent-p t
                ;; js2-bounce-indent-p t
                js2-indent-on-enter-key t
                js2-global-externs (list "window" "module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "jQuery" "$")
                js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil
                flycheck-eslintrc "~/.eslintrc")
          (add-hook 'js-mode-hook
                    (lambda ()
                      (flycheck-mode 1)
                      (tern-mode 1)
                      (set (make-local-variable 'company-backends) '((company-tern))))))
  :config (progn
            (bind-key "<f8>" 'nodejs-repl js2-mode-map)
            (bind-key "C-c C-f" 'web-beautify-js js2-mode-map)
            (bind-key "C-c C-i" 'import-js-fix js2-mode-map)
            (bind-key "C-c i" 'import-js-goto js2-mode-map)
            (bind-key "C-c C-u" 'import-js-import js2-mode-map)
            (bind-key "C-c C-/" 'my/js2-toggle-indent js2-mode-map)
            (bind-key "C-c /" 'my/js2-toggle-indent js2-mode-map)))


;; TERN
(use-package tern
  :ensure t
  :init(progn
         (defun delete-tern-process ()
           (interactive)
           (if (get-process "Tern") (delete-process (get-process "Tern")))
           (if (get-process "Tern<1>") (delete-process (get-process "Tern<1>")))
           (if (get-process "Tern<2>") (delete-process (get-process "Tern<2>")))
           (if (get-process "Tern<3>") (delete-process (get-process "Tern<3>")))
           (if (get-process "Tern<4>") (delete-process (get-process "Tern<4>")))
           (if (get-process "Tern<5>") (delete-process (get-process "Tern<5>")))))

  :config(progn
           (unbind-key "C-c C-c" tern-mode-keymap)
           (bind-key "C-c C-l" 'delete-tern-process tern-mode-keymap)
           (bind-key "C-c C-," 'tern-find-definition tern-mode-keymap)
           (bind-key "C-c C-\'" 'tern-pop-find-definition tern-mode-keymap)
           (bind-key "C-c C-\." 'tern-find-definition-by-name tern-mode-keymap)
           (bind-key "C-c C-t" 'tern-get-type tern-mode-keymap)
           (bind-key "C-c C-d" 'tern-get-docs tern-mode-keymap)))
  

(use-package company-tern :ensure t)


(use-package import-js
  :ensure t :defer t)


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


;; COFFEESCRIPT
(use-package coffee-mode
  :ensure t :defer t
  :init(progn
         (setq coffee-tab-width 2)))


;; NODE
(use-package nodejs-repl
  :ensure t :defer t
  :init(progn
         (defun nodejs-repl--sanitize-code (text)
           "Avoid conflicts with REPL special constructs: _ and .command"
           (->> text
                ;; If there is a chained call on a new line, move the dot to the previous line;
                ;; the repl executes lines eagerly and interprets " .something" as a REPL command
                (replace-regexp-in-string "\n\\(\\s-*\\)\\.\\(\\w+\\)" ".\n\\1\\2")
                ;; Replace _ with __ because underscore is a special thing in the REPL
                (replace-regexp-in-string "\\_<_\\." "__.")
                ;; Replace var _ = require ... with var __ = ...
                (replace-regexp-in-string "var\\s-+_\\s-+=" "var __ =")))

         (defun nodejs-repl-eval-region (start end)
           "Evaluate the region specified by `START' and `END'."
           (let ((proc (get-process nodejs-repl-process-name)))
             (comint-simple-send proc
                                 (nodejs-repl--sanitize-code
                                  (buffer-substring-no-properties start end)))))

         (defun nodejs-repl-eval-node (node)
           "Evaluate `NODE', a `js2-mode' node."
           (let ((beg (js2-node-abs-pos node))
                 (end (js2-node-abs-end node)))
             (nodejs-repl-eval-region beg end)))

         (defun nodejs-repl--find-current-or-prev-node (pos &optional include-comments)
           "Locate the first node before `POS'.  Return a node or nil.
If `INCLUDE-COMMENTS' is set to t, then comments are considered
valid nodes.  This is stupid, don't do it."
           (let ((node (js2-node-at-point pos (not include-comments))))
             (if (or (null node)
                     (js2-ast-root-p node))
                 (unless (= 0 pos)
                   (nodejs-repl--find-current-or-prev-node (1- pos) include-comments))
               node)))

         (defun nodejs-repl-eval-function ()
           "Evaluate the current or previous function."
           (interactive)
           (let* ((fn-above-node (lambda (node)
                                   (js2-mode-function-at-point (js2-node-abs-pos node))))
                  (fn (funcall fn-above-node
                               (nodejs-repl--find-current-or-prev-node
                                (point) (lambda (node)
                                          (not (null (funcall fn-above-node node))))))))
             (unless (null fn)
               (nodejs-repl-eval-node fn))))

         (defun nodejs-repl-eval-first-stmt (pos)
           "Evaluate the first statement found from `POS' by `js2-mode'.
If this statement is a block statement, its first parent
statement is found.  This will be either a function declaration,
function call, or assignment statement."
           (let ((node (js2-mode-find-first-stmt (nodejs-repl--find-current-or-prev-node pos))))
             (cond
              ((js2-block-node-p node) (nodejs-repl-eval-node (js2-node-parent-stmt node)))
              ((not (null node)) (nodejs-repl-eval-node node)))))

         (defun nodejs-repl-eval-dwim ()
           "Heuristic evaluation of JS code in a NodeJS repl.
Evaluates the region, if active, or the first statement found at
or prior to the point.
If the point is at the end of a line, evaluation is done from one
character prior.  In many cases, this will be a semicolon and will
change what is evaluated to the statement on the current line."
           (interactive)
           (cond
            ((use-region-p) (nodejs-repl-eval-region (region-beginning) (region-end)))
            ((= (line-end-position) (point)) (nodejs-repl-eval-first-stmt (1- (point))))
            (t (nodejs-repl-eval-first-stmt (point)))))

         (defun nodejs-repl-eval-buffer (&optional buffer)
           "Evaluate the current buffer or the one given as `BUFFER'.
`BUFFER' should be a string or buffer."
           (interactive)
           (let ((buffer (or buffer (current-buffer))))
             (with-current-buffer buffer
               (nodejs-repl-eval-region (point-min) (point-max)))))
         
         (defun nodejs-switch-to-shell ()
           "Switch to inferior js process buffer."
           (interactive)
           (switch-to-buffer-other-window "*nodejs*"))


         (defun nodejs-clear ()
           "Switch to inferior js process buffer."
           (interactive)
           (let ((buffer-name (current-buffer)))
             (switch-to-buffer-other-window "*nodejs*")
             (end-of-buffer)
             (insert "Object.keys(require.cache).forEach(function(key) { delete require.cache[key] })")
             (comint-send-input)
             (switch-to-buffer-other-window buffer-name)))

         (provide 'nodejs-repl-eval))

  :config (progn
            (add-hook 'js2-mode-hook (lambda ()
                                       (bind-key "C-c C-z" 'nodejs-switch-to-shell js2-mode-map)
                                       (bind-key "C-c C-g" 'nodejs-clear js2-mode-map)
                                       (bind-key "C-c C-c" 'nodejs-repl-eval-dwim js2-mode-map)
                                       (bind-key "C-x C-e" 'nodejs-repl-eval-node js2-mode-map)
                                       (bind-key "C-c C-r" 'nodejs-repl-eval-region js2-mode-map)
                                       (bind-key "C-c C-f" 'nodejs-repl-eval-function js2-mode-map)
                                       (bind-key "C-c C-k" 'nodejs-repl-eval-buffer js2-mode-map)))))


;; TYPESCRIPT
(use-package typescript-mode
  :ensure t :defer t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode ))
  :init (progn
          (add-hook 'typescript-mode-hook
                    (lambda ()
                      (tide-setup)
                      (flycheck-mode t)
                      (setq flycheck-check-syntax-automatically '(save mode-enabled))
                      ;; (eldoc-mode t)
                      (set (make-local-variable 'company-backends) '((company-tide))))))
  :config (progn
            (bind-key "C-c C-d" 'tide-documentation-at-point typescript-mode-map)
            (bind-key "C-c C-," 'tide-jump-to-definition typescript-mode-map)
            (bind-key "C-c C-\'" 'tide-jump-back typescript-mode-map)
            (bind-key "C-c C-f" 'tide-format typescript-mode-map)
            (bind-key "C-c C-a" 'tide-references typescript-mode-map)
            (bind-key "C-c C-r" 'tide-rename-symbol typescript-mode-map)
            (bind-key "C-c C-s" 'tide-goto-reference typescript-mode-map)
            (bind-key "C-c C-l" 'tide-restart-server typescript-mode-map)))



;; TIDE
(use-package tide
  :ensure t :defer t)



;; ELM
(use-package elm-mode
  :ensure t
  :init (progn
          (add-hook 'elm-mode-hook
                    (lambda()
                      (eldoc-mode -1)
                      (set (make-local-variable 'company-backends) '((company-elm company-dabbrev-code)))))))



;; ELISP
(use-package emacs-lisp-mode
  :defer t
  :init (progn
          (add-hook 'emacs-lisp-mode-hook
                    (lambda ()
                      (set (make-local-variable 'company-backends) '((company-elisp company-dabbrev-code)))))
          (bind-key "C-c C-c" 'eval-defun emacs-lisp-mode-map)
          (bind-key "C-c C-r" 'eval-region emacs-lisp-mode-map)
          (bind-key "C-c C-k" 'eval-buffer emacs-lisp-mode-map)
          (bind-key "C-c C-e" 'eval-last-sexp emacs-lisp-mode-map)
          (bind-key "C-c e" 'eval-last-sexp emacs-lisp-mode-map)
          (bind-key "C-c C-f" 'eval-last-sexp emacs-lisp-mode-map)))


;; CLOJURE
(use-package clojure-mode
  :defer t
  :config
  (defun my/clojure-mode-defaults ()
    (bind-key "<f8>" 'cider-jack-in)
    (bind-key "S-<f8>" 'cider-jack-in-clojurescript)
    (bind-key "C-c d l" 'clojure-cheatsheet))
  (add-hook 'clojure-mode-hook 'my/clojure-mode-defaults))


(use-package cider
  :ensure t 
  :init (progn
          (defun my/cider-load-buffer (&optional BUFFER)
            (interactive)
            (save-buffer)
            (cider-load-buffer BUFFER))
          ;; (add-hook 'cider-mode-hook #'company-mode)
          (add-hook 'cider-mode-hook
                    (lambda ()
                      (set (make-local-variable 'company-backends) '(company-capf)))))
  :config (progn
            (bind-key "C-c C-k" 'my/cider-load-buffer cider-mode-map)))


(use-package clj-refactor
  :ensure t :defer t
  :init(progn
         (setq cljr-suppress-middleware-warnings t
               cljr-auto-clean-ns nil
               cljr-auto-sort-ns nil
               cljr-auto-eval-ns-form nil)
         (add-hook 'clojure-mode-hook (lambda ()
                                        (clj-refactor-mode 1)
                                        (cljr-add-keybindings-with-prefix "C-c r")))))

;; COMMON LISP
(use-package slime
  :ensure t :defer t
  :init (progn
          (setq inferior-lisp-program "/usr/bin/sbcl")
          (slime-setup '(slime-fancy)))
  :config (progn
            (bind-key "<f8>" 'slime slime-mode-map)))


;; LUA
(use-package company-lua :ensure t)
(use-package lua-mode
  :ensure t :defer t
  :init(progn
         (setq lua-indent-level 2
               lua-prefix-key "C-c")
         (add-hook 'lua-mode-hook
                   (lambda ()
                     (set (make-local-variable 'company-backends) '((company-lua company-dabbrev-code)))))
         (bind-key "C-c C-c" 'lua-send-buffer lua-mode-map)
         (bind-key "C-c C-d" 'lua-search-documentation lua-mode-map)
         (bind-key "C-c C-k" 'lua-send-defun lua-mode-map)
         (bind-key "C-c C-r" 'lua-send-region lua-mode-map))
  :config(progn
           (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
           (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))))


;; ELIXIR
(use-package alchemist
  :ensure t
  :init(progn
         (setq alchemist-goto-elixir-source-dir "/usr/local/lib/elixir/"
               alchemist-goto-erlang-source-dir "/usr/local/lib/elixir/lib")
         (add-hook 'alchemist-mode-hook
                   (lambda ()
                     (set (make-local-variable 'company-backends) '((alchemist-company)))))
         (add-hook 'alchemist-iex-mode-hook
                   (lambda ()
                     (company-mode-on)
                     (set (make-local-variable 'company-backends) '((alchemist-company))))))
  :config (progn
            (bind-key "<f8>" 'alchemist-iex-run alchemist-mode-map)
            (bind-key "<f9>" 'alchemist-iex-project-run alchemist-mode-map)
            (bind-key "C-c C-c" 'alchemist-iex-send-last-sexp alchemist-mode-map)
            (bind-key "C-x C-e" 'alchemist-iex-send-last-sexp alchemist-mode-map)
            (bind-key "C-c C-r" 'alchemist-iex-send-region alchemist-mode-map)
            (bind-key "C-c C-k" 'alchemist-iex-compile-this-buffer alchemist-mode-map)
            (bind-key "C-c C-l" 'alchemist-iex-reload-module alchemist-mode-map)
            (bind-key "C-c C-z" 'alchemist-iex-run alchemist-mode-map)
            (bind-key "C-c C-," 'alchemist-goto-definition-at-point alchemist-mode-map)
            (bind-key "C-c C-'" 'alchemist-goto-jump-back alchemist-mode-map)
            (bind-key "C-l" 'alchemist-iex-clear-buffer alchemist-iex-mode-map)))



;; SHELL SH
(use-package company-shell :ensure t)
(use-package sh-mode
  :init (progn
          (add-hook 'sh-mode-hook
                    (lambda ()
                      (company-quickhelp-mode -1)
                      (set (make-local-variable 'company-backends) '((company-dabbrev-code company-shell)))))))



;; C C++
(use-package c-mode-common-hook
  :init(progn
         (setq-default c-basic-offset 4 c-default-style "linux")
         (setq-default tab-width 4 indent-tabs-mode t)
         (setq irony-supported-major-modes '(c++-mode c-mode objc-mode php-mode))
         (add-hook 'c-mode-hook
                   (lambda ()
                     (irony-mode 1)
                     (irony-eldoc 1)
                     (flycheck-mode 1)
                     (flycheck-irony-setup)
                     (set (make-local-variable 'company-backends) '((company-irony company-irony-c-headers)))  
                     (bind-key "C-c C-." 'semantic-ia-fast-jump c-mode-map)
                     (unbind-key "C-d" c-mode-map)
                     (unbind-key "C-c C-d" c-mode-map)))
         (add-hook 'c++-mode-hook
                   (lambda ()
                     (setq flycheck-gcc-language-standard "c++11")
                     (irony-mode 1)
                     (irony-eldoc 1)
                     (flycheck-mode 1)
                     (flycheck-irony-setup)
                     (set (make-local-variable 'company-backends) '((company-irony company-irony-c-headers)))  
                     (bind-key "C-c C-." 'semantic-ia-fast-jump c++-mode-map)
                     (unbind-key "C-d" c++-mode-map)
                     (unbind-key "C-c C-d" c++-mode-map)))))


(use-package irony
  :ensure t :defer t
  :init (progn
          (custom-set-variables '(irony-additional-clang-options '("-std=c++11")))))

(use-package company-irony
  :ensure t :defer t)

(use-package irony-eldoc
  :ensure t :defer t)

(use-package company-irony-c-headers
  :ensure t :defer t)

(use-package flycheck-irony
  :ensure t :defer t)


;; GO
(use-package go-mode
  :ensure t :defer t
  :init (progn
          (setenv "GOROOT" "/opt/go")
          (setenv "GOPATH" "/home/vince/.go")
          (setq gofmt-command "goimports")
          (add-hook 'go-mode-hook
                    (lambda ()
                      (add-hook 'before-save-hook 'gofmt-before-save)
                      (flycheck-mode 1))))
  :config (progn
            (bind-key "C-c C-a" 'go-goto-imports go-mode-map)            
            (bind-key "C-c C-o" 'go-import-add go-mode-map)            
            (bind-key "C-c C-e" 'go-remove-unused-imports go-mode-map)
            (bind-key "C-c C-'" 'go-goto-function go-mode-map)
            (bind-key "C-c C-," 'go-goto-arguments go-mode-map)
            (bind-key "C-c C-." 'go-goto-function-name go-mode-map)
            (bind-key "C-c C-p" 'go-goto-return-values go-mode-map)
            (bind-key "C-c C-d" 'godef-describe go-mode-map)
            (bind-key "C-c C-l" 'godef-jump go-mode-map)
            (bind-key "C-c C-/" 'godef-jump-other-window go-mode-map)
            (bind-key "C-c C--" 'godoc-at-point go-mode-map)
            (bind-key "C-c C-\\" 'godoc go-mode-map)
            (bind-key "C-c C-f" 'gofmt go-mode-map)))

(use-package company-go
  :ensure t :defer t)


(use-package go-eldoc
  :ensure t
  :init (progn
          (add-hook 'go-mode-hook 'go-eldoc-setup)))


;; RUST
(use-package rust-mode
  :ensure t :defer t
  :init (progn
          (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
          (add-hook 'rust-mode-hook (lambda ()
                                      (flycheck-mode 1)
                                      (racer-mode)
                                      (set (make-local-variable 'company-backends) '((company-racer)))))
          (add-hook 'racer-mode-hook 'eldoc-mode)
          (add-hook 'racer-mode-hook 'cargo-minor-mode)
          
          )
  :config (progn
            (bind-key "C-c C-." 'racer-find-definition rust-mode-map)
            (bind-key "C-c C-d" 'cargo-process-doc rust-mode-map)
            (bind-key "C-c C-r" 'cargo-process-run rust-mode-map)
            (bind-key "C-c C-n" 'cargo-process-new rust-mode-map)
            (bind-key "C-c C-t" 'cargo-process-test rust-mode-map)
            (bind-key "C-c C-b" 'cargo-process-build rust-mode-map)
            (bind-key "C-c C-l" 'cargo-process-clean rust-mode-map)
            (bind-key "C-c C-e" 'cargo-process-bench rust-mode-map)
            (bind-key "C-c C-u" 'cargo-process-update rust-mode-map)
            (bind-key "C-c C-c" 'cargo-process-repeat rust-mode-map)
            (bind-key "C-c C-s" 'cargo-process-search rust-mode-map)
            (bind-key "C-c C-T" 'cargo-process-current-test rust-mode-map)
            (bind-key "C-c C-o" 'cargo-process-current-file-tests rust-mode-map)))

(use-package racer
  :ensure t :defer t)

(use-package company-racer
  :ensure t :defer t)

(use-package cargo
  :ensure t :defer t)

(use-package flycheck-rust
  :ensure t :defer t)


;; PHP
;;(use-package php-mode :ensure t)
;;(use-package company-php
;;  :ensure t
;;  :init (progn
;;          (add-hook 'php-mode-hook
;;                    (lambda ()
;;                      (set (make-local-variable 'company-backends) '((company-ac-php-backend)))
;;                      (flycheck-mode 1)
;;                      (add-hook 'before-save-hook
;;                                (lambda ()
;;                                  (when (eq major-mode 'php-mode)
;;                                    (ac-php-remake-tags)))))))
;;  :config (progn
;;            (bind-key "C-SPC" 'ac-complete-php php-mode-map)
;;            (bind-key "C-c s" 'ac-php-remake-tags-all php-mode-map)
;;            (bind-key "C-c C-S" 'ac-php-remake-tags php-mode-map)
;;            (bind-key "C-c C-." 'ac-php-find-symbol-at-point php-mode-map)
;;            (bind-key "C-c ." 'ac-php-find-symbol-at-point php-mode-map)
;;            (bind-key "C-c C-," 'ac-php-location-stack-back php-mode-map)
;;            (bind-key "C-c ," 'ac-php-location-stack-back php-mode-map)
;;			(unbind-key "C-d" php-mode-map)
;;			(unbind-key "M-q" php-mode-map)
;;			(unbind-key "C-." php-mode-map)))





;; (use-package php-extras
;;   :ensure t :defer t)

;; STUFF
;; remove window decoration
(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (set-face-inverse-video-p 'vertical-border nil)
  (scroll-bar-mode -1))

;; SCROOL
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)


(fringe-mode 0) ;; turn off left and right fringe cols
(fset 'yes-or-no-p 'y-or-n-p);; replace yes to y
(setq confirm-nonexistent-file-or-buffer nil)

;; tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; insert ret if last line
(setq next-line-add-newlines nil)

;; scratch message
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)

;; replace dabbrev by hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; save
 (setq auto-save-default nil
       auto-save-interval 0)

;; kill process no prompt
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;; desktop mode
(desktop-save-mode)

;; search regex
(setq case-fold-search nil)

;; save on focus out
(defun my-save-out-hook ()
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'my-save-out-hook)

;; save all no prompt
(defun my-save-all ()
  (interactive)
  (save-some-buffers t))

;; mark
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))
(setq set-mark-command-repeat-pop t)



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


(defun run-in-eshell (code)
  (interactive "M")
  (setq last-executed-code code)
  (let ((current (current-buffer))
        (shell-name "*eshell*"))
    (when (not (get-buffer shell-name ))
      (eshell))
    (when (not (string-equal (buffer-name (current-buffer)) shell-name))
      (switch-to-buffer-other-window (get-buffer shell-name)))
    (end-of-buffer)
    (eshell-kill-input)
    (insert code)
    (eshell-send-input)
    (when (not (string-equal (buffer-name current) shell-name))
      (switch-to-buffer-other-window current))))


(defun re-run-in-eshell (&optional dt)
  (interactive "P")
  (save-buffer)
  (run-in-eshell last-executed-code))



(defun eshell-dwim (&optional create)
  (interactive "P")
  (let ((eshell-buf-list (identity
                          (sort
                           (my-filter-shell (lambda (x) (string-match "^\\*eshell\\*" (buffer-name x))) (buffer-list))
                           #'(lambda (a b) (string< (buffer-name a) (buffer-name b)))))))
    (setq eshell-buffer-name
          (if (string-match "^\\*eshell\\*" (buffer-name))
              (buffer-name (get-buffer (cadr (member (buffer-name) (mapcar (function buffer-name) (append eshell-buf-list eshell-buf-list))))))
            "*eshell*"))
    (if create
        (setq eshell-buffer-name (eshell "new"))
      (eshell))))


(defun my-dirname-buffer ()
  (interactive)
  (let ((dirname (file-name-directory (buffer-file-name))))
    (progn
      (message dirname)
      (kill-new dirname))))


(defun my-open-with (arg)
  (interactive "P")
  (when buffer-file-name
    (shell-command (concat
                    (cond
                     ((and (not arg) (eq system-type 'darwin)) "open")
                     ((and (not arg) (member system-type '(gnu gnu/linux gnu/kfreebsd))) "xdg-open")
                     (t (read-shell-command "Open current file with: ")))
                    " "
                    (shell-quote-argument buffer-file-name)))))

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'"
  (interactive)
  (let ((ξpath (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (let (p0 p1 p2)
                   (setq p0 (point))
                   ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
                   (skip-chars-backward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
                   (setq p1 (point))
                   (goto-char p0)
                   (skip-chars-forward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
                   (setq p2 (point))
                   (goto-char p0)
                   (buffer-substring-no-properties p1 p2)))))
    (if (string-match-p "\\`https?://" ξpath)
        (browse-url ξpath)
      (progn ; not starting “http://”
        (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" ξpath)
            (progn
              (let (
                    (ξfpath (match-string 1 ξpath))
                    (ξline-num (string-to-number (match-string 2 ξpath))))
                (if (file-exists-p ξfpath)
                    (progn
                      (find-file ξfpath)
                      (goto-char 1)
                      (forward-line (1- ξline-num)))
                  (progn
                    (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξfpath))
                      (find-file ξfpath))))))
          (progn
            (if (file-exists-p ξpath)
                (find-file ξpath)
              (if (file-exists-p (concat ξpath ".el"))
                  (find-file (concat ξpath ".el"))
                (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξpath))
                  (find-file ξpath ))))))))))



(defun my-sudo ()
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))


(defun my-kill-all-dired-buffers ()
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'dired-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count))))


(defun shell-buffer ()
  (interactive)
  (switch-to-buffer "*quickrun*"))

(defun scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun shell-command-buffer ()
  (interactive)
  (switch-to-buffer "*Shell Command Output*"))

(defun python-buffer ()
  (interactive)
  (switch-to-buffer "*Python*"))


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

(defun my-kill-buffer ()
  (interactive)
  (when (not (string-match "^\*.*\*$" (buffer-name (current-buffer))))
    (save-buffer))
  (kill-this-buffer))


(defun my-previous-user-buffer ()
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (or
                 (string-equal "*" (substring (buffer-name) 0 1))
                 (string-equal "dired-mode" (message "%s" major-mode)))
                (< i 20))
      (setq i (1+ i)) (previous-buffer) )))

(defun my-next-user-buffer ()
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (or
                 (string-equal "*" (substring (buffer-name) 0 1))
                 (string-equal "dired-mode" (message "%s" major-mode)))
                (< i 20))
      (setq i (1+ i)) (next-buffer) )))


(defun my-previous-user-dired-buffer ()
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and
            (not (string-equal "dired-mode" (message "%s" major-mode)))
            (< i 20))
      (setq i (1+ i)) (previous-buffer))))

(defun my-next-user-dired-buffer ()
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and
            (not (string-equal "dired-mode" (message "%s" major-mode)))
            (< i 20))
      (setq i (1+ i)) (next-buffer))))



(defun my-indent-shift-left (start end &optional count)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count 2))
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
                  2))
    (indent-rigidly start end count)))


(defun my-toogle-case ()
    (interactive)
    (if case-fold-search
        (progn
          (setq case-fold-search nil)
          (message "toogle off"))
      (progn
        (setq case-fold-search t)
        (message "toggle on"))))


(defmacro def-pairs (pairs)
  `(progn
     ,@(loop for (key . val) in pairs
             collect
             `(defun ,(read (concat
                             "wrap-with-"
                             (prin1-to-string key)
                             "s"))
                  (&optional arg)
                (interactive "p")
                (sp-wrap-with-pair ,val)))))
(def-pairs ((paren        . "(")
            (bracket      . "[")
            (brace        . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote   . "`")))


(defun my-revert-buffer-no-confirm ()
  (interactive)
  (revert-buffer t t))

(defun my-close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


(defun my-split-verticaly ()
  (interactive)
  (delete-other-windows)
  (when (string-match "^\\*eshell\\*" (buffer-name))
    (my-previous-user-buffer))
  (split-window-vertically)
  (windmove-down)
  (when (string-match "^\\*eshell\\*" (buffer-name))
    (my-previous-user-buffer))
  (my-previous-user-buffer)
  (windmove-up)
  (balance-windows))

(defun my-split-horizontaly ()
  (interactive)
  (delete-other-windows)
  (when (string-match "^\\*eshell\\*" (buffer-name))
    (my-next-user-buffer))
  (split-window-horizontally)
  (windmove-right)
  (when (string-match "^\\*eshell\\*" (buffer-name))
    (my-next-user-buffer))
  (my-previous-user-buffer)
  (windmove-left)
  (balance-windows))

(defun split-3-0 ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (windmove-right)
  (split-window-horizontally)
  (windmove-right)
  (windmove-left)
  (windmove-left)
  (balance-windows))


(defun split-2-1 ()
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
  (enlarge-window 18)
  (windmove-down)
  (shell-dwim)
  (windmove-up)
  (split-window-horizontally)
  (windmove-right)
  (windmove-left))


(defun split-2-1-eshell ()
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
  (enlarge-window 18)
  (windmove-down)
  (eshell-dwim)
  (windmove-up)
  (split-window-horizontally)
  (windmove-right)
  (windmove-left))


(defun split-2-2 ()
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
  (windmove-down)
  (setq eshell-buffer-name "*eshell*")
  (shell-dwim)
  (split-window-horizontally)
  (windmove-right)
  (shell-dwim)
  (windmove-left)
  (windmove-up)
  (split-window-horizontally)
  (windmove-right)
  (windmove-left)
  (balance-windows)
  (enlarge-window 19))


(defun split-2-2-eshell ()
  (interactive)
  (delete-other-windows)
  (setq eshell-buffer-name "*eshell*")
  (eshell-dwim)
  (split-window-vertically)
  (windmove-down)
  (eshell 2)
  (split-window-horizontally)
  (windmove-right)
  (eshell 3)
  (windmove-left)
  (windmove-up)
  (split-window-horizontally)
  (windmove-right)
  (eshell 4)
  (windmove-left)
  (balance-windows))


(defun split-2-2-shell ()
  (interactive)
  (delete-other-windows)
  (shell 1)
  (split-window-vertically)
  (windmove-down)
  (shell 2)
  (split-window-horizontally)
  (windmove-right)
  (shell 3)
  (windmove-left)
  (windmove-up)
  (split-window-horizontally)
  (windmove-right)
  (shell 4)
  (windmove-left)
  (balance-windows))


(defun split-3-1 ()
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
  (windmove-down)
  (eshell-dwim)
  (windmove-up)
  (split-window-horizontally)
  (windmove-right)
  (my-previous-user-buffer)
  (split-window-horizontally)
  (windmove-right)
  (my-previous-user-buffer)
  (windmove-left)
  (windmove-left)
  (balance-windows)
  (enlarge-window 19))

(defun split-3-3 ()
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
  (windmove-down)
  (setq eshell-buffer-name "*eshell*")
  (eshell-dwim)
  (split-window-horizontally)
  (windmove-right)
  (eshell 2)
  (split-window-horizontally)
  (windmove-right)
  (eshell 3)
  (windmove-left)
  (windmove-left)
  (windmove-up)
  (split-window-horizontally)
  (windmove-right)
  (split-window-horizontally)
  (windmove-right)
  (windmove-left)
  (windmove-left)
  (balance-windows)
  (enlarge-window 19))




;; GENERAL KEYBINDING
;; MARK COMMAND, COMPLETE, YAS, TAB, SAVE
(bind-key "M-SPC" 'set-mark-command)
(bind-key "C-SPC" 'company-complete)
(bind-key "M-C--" 'my-indent-shift-left)
(bind-key "M-C-\\" 'my-indent-shift-right)
(bind-key "<backtab>" 'my-indent-shift-left)
(bind-key "C--" 'yas-expand)
(bind-key "M-\\" 'flycheck-mode)
(bind-key "C-M-\\" 'global-flycheck-mode)
(bind-key* "C-a" 'mark-whole-buffer)
(bind-key "<M-return>" 'smart-ret)
(bind-key "<S-return>" 'smart-ret-reverse)
(bind-key "<escape>" 'keyboard-espace-quit)
(bind-key "M-m" 'emmet-expand-line)
(bind-key "C-c w" 'emmet-wrap-with-markup)
(bind-key "C-c C-w" 'emmet-wrap-with-markup)
(bind-key "C-s" 'my-save-all)
(bind-key "C-x C-s" 'save-buffer)
(bind-key "C-x s" 'my-revert-buffer-no-confirm)
(unbind-key "M-<down-mouse-1>")
(bind-key* "M-<mouse-1>" 'mc/add-cursor-on-click)


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
(bind-key "M-G" 'my-backward-block)
(bind-key "M-R" 'my-forward-block)
(bind-key "M-d" 'my-beginning-of-line-or-block)
(bind-key* "M-D" 'end-of-line)
(bind-key "M-b" 'beginning-of-buffer)
(bind-key "M-B" 'end-of-buffer)
(bind-key* "C-M-v" 'sp-next-sexp)
(bind-key* "C-M-w" 'sp-previous-sexp)
(bind-key* "M-v" 'sp-end-of-sexp)
(bind-key* "M-w" 'sp-beginning-of-sexp)

(bind-key* "M-V" 'sp-down-sexp)
(bind-key* "M-W" 'sp-backward-down-sexp)
(bind-key* "C-M-S-v" 'sp-beginning-of-next-sexp)
(bind-key* "C-M-S-w" 'sp-beginning-of-previous-sexp)
(bind-key* "C-S-w" 'sp-up-sexp)
(bind-key* "C-S-v" 'sp-backward-up-sexp)
(bind-key "M-Z" 'scroll-other-window)
(bind-key "C-M-Z" 'scroll-other-window-down)
(bind-key "C-\\" 'xah-open-file-at-cursor)
(bind-key* "C-M-h" 'elscreen-previous)
(bind-key* "C-M-n" 'elscreen-next)

(bind-key "C-M-c" 'occur)
(bind-key "M-H" 'sp-backward-sexp)
(bind-key "M-N" 'sp-forward-sexp)
(bind-key* "M-9" 'sp-splice-sexp)
(bind-key* "M-0" 'sp-rewrap-sexp)
(bind-key* "M-[" 'sp-forward-barf-sexp)
(bind-key* "M-]" 'sp-forward-slurp-sexp)
(bind-key* "M-{" 'sp-backward-barf-sexp)
(bind-key* "M-}" 'sp-backward-slurp-sexp)
(bind-key "C-S-j" 'sp-join-sexp)


;; DELETE KEY
(bind-key* "M-e" 'backward-delete-char-untabify)
(bind-key* "M-u" 'delete-char)
(bind-key "M-E" 'sp-backward-kill-sexp)
(bind-key "M-U" 'sp-kill-sexp)
(bind-key* "M-." 'backward-kill-word)
(bind-key* "M->" 'zap-to-char)
(bind-key* "M-p" 'kill-word)
(bind-key* "M-i" 'kill-line)
(bind-key* "M-I" 'my-kill-line-backward)
(bind-key* "M-y" 'undo-tree-redo)

;; COPY, CUT, PASTE, REDO, UNDO ,DUPLICATE, JOIN
(bind-key "M-q" 'my-cut-line-or-region)
(bind-key "M-Q" 'my-cut-line-or-region)
(bind-key* "M-j" 'my-copy-line-or-region)
(bind-key* "M-J" 'sp-backward-copy-sexp)
(bind-key* "C-M-J" 'sp-copy-sexp)
(bind-key* "M-k" 'yank)
(bind-key "M-K" 'yank-pop)
(bind-key* "M-;" 'undo-tree-undo)
(bind-key* "M-:" 'undo-tree-redo)
(bind-key* "C-z" 'undo-tree-undo)
(bind-key "C-S-z" 'undo-tree-redo)
(bind-key "C-x u" 'undo-tree-visualize)
(bind-key "C-d" 'duplicate-current-line-or-region)
(bind-key "C-j" 'join-line-or-lines-in-region)


;; POP, GOTO, INFO, SCALE, CAMEL, RECENTER, REPLACE
(bind-key* "M-f" 'goto-last-change)
(bind-key* "M-F" 'goto-last-change-reverse)
(bind-key* "C-S-s" 'ido-write-file)
(bind-key "C-l" 'goto-line)
(bind-key* "C-=" 'text-scale-increase)
(bind-key* "C-+" 'text-scale-decrease)
(bind-key* "M-z" 'my-toggle-letter-case)

;; FRAME CLOSE BUFFER, COMMENT
(bind-key* "C-b" 'make-frame-command)
(bind-key "C-w" 'my-kill-buffer)
(bind-key* "C-S-w" 'my-kill-all-dired-buffers)
(bind-key* "C-x C-w" 'my-close-all-buffers)
(bind-key* "M-;" 'comment-dwim-2)
(bind-key* "M-:" 'comment-box)

;; COMMAND, SHELL, RUN, EMMET
(bind-key* "M-a" 'helm-M-x)
(bind-key* "M-A" 'shell-command)
(bind-key* "M-C-a" 'shell-command-on-region)
(bind-key* "M-C-S-a" 'eval-expression)
(bind-key* "M-1" 'shell-dwim)
(bind-key* "M-!" 'eshell-dwim)
(bind-key* "S-<f1>" 'shell-buffer)
(bind-key* "<f1>" 'scratch-buffer)
(bind-key* "<f2>" 'shell-command-buffer)
(bind-key "M-/" 'neotree-show)
(bind-key "M-=" 'neotree-toggle)
(bind-key "M-)" 'balance-windows)
(bind-key* "<f4>" 'kmacro-end-or-call-macro-repeat)
(bind-key "S-<f5>" 'compile)
(bind-key "<f5>" 'recompile)
(bind-key "S-<f6>" 'run-in-eshell)
(bind-key "<f6>" 're-run-in-eshell)
(bind-key* "<f7>" 'helm-bookmarks)
(bind-key "S-<f9>" 'quick-calc)
(bind-key "<f9>" 'calc)
(bind-key* "<f12>" 'toggle-frame-fullscreen)
(bind-key* "C-o" 'helm-find-files)
(bind-key* "C-S-o" 'helm-recentf)
(bind-key "C-p" 'helm-semantic-or-imenu)
(bind-key* "C-y" 'helm-show-kill-ring)
(bind-key* "C-f" 'helm-projectile-ag)
(bind-key* "C-F" 'helm-ag)
(bind-key "C-h a" 'helm-apropos)
(bind-key "C-h o" 'helm-man-woman)
(bind-key* "M--" 'yafolding-toggle-element)
(bind-key* "M-_" 'yafolding-toggle-all)
(global-set-key (kbd "M-o") 'projectile-find-file)
(global-set-key (kbd "C-e") 'helm-buffers-list)

;; DIRED
(bind-key "C-x j" 'dired-jump)
(bind-key "C-x C-j" 'find-name-dired)
(bind-key "C-x J" 'find-grep-dired)
(bind-key "C-x C-J" 'find-lisp-find-dired)
(bind-key "C-x M-j" 'locate)
(bind-key "C-x M-J" 'locate-with-filter)
(bind-key* "C-/" 'projectile-dired)

;; UNWRAP
(bind-key "C-c ("  'wrap-with-parens)
(bind-key "C-c ["  'wrap-with-brackets)
(bind-key "C-c {"  'wrap-with-braces)
(bind-key "C-c '"  'wrap-with-single-quotes)
(bind-key "C-c \"" 'wrap-with-double-quotes)
(bind-key "C-c _"  'wrap-with-underscores)
(bind-key "C-c `"  'wrap-with-back-quotes)

;; HELM SWOOP
(bind-key "C-r" 'helm-swoop)
(bind-key "C-M-r" 'helm-multi-swoop-current-mode)
(bind-key "C-S-r" 'helm-swoop-back-to-last-point)
(bind-key "M-7" 'helm-multi-swoop)
(bind-key "M-*" 'helm-multi-swoop-all)
(bind-key "M-8" 'helm-multi-swoop-current-mode)

;; MAGIT
(bind-key "C-x g" 'magit-status)

;; SELECTION
(bind-key "M-l" 'my-select-current-line)
(bind-key "M-L" 'my-select-current-block)
(bind-key "C-M-l" 'er/mark-defun)
(bind-key* "M-s" 'er/expand-region)
(bind-key* "M-S" 'er/mark-inside-pairs)
(bind-key* "C-M-s" 'er/mark-symbol)
(bind-key* "C-S-M-s" 'er/mark-inner-tag)


;; BUFFER SWITCHING ERROR
(bind-key* "C-S-e" 'ibuffer)
(bind-key* "M-'" 'my-previous-user-buffer)
(bind-key* "M-," 'my-next-user-buffer)
(bind-key* "M-\"" 'my-previous-user-dired-buffer)
(bind-key* "M-<" 'my-next-user-dired-buffer)
(bind-key* "C-M-'" 'my-previous-user-dired-buffer)
(bind-key* "C-M-," 'my-next-user-dired-buffer)

(bind-key* "C-'" 'smartscan-symbol-go-backward)
(bind-key* "C-," 'smartscan-symbol-go-forward)
(bind-key* "C-\"" 'previous-error)
(bind-key* "C-<" 'next-error)
(bind-key* "C->" 'flycheck-list-errors)

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




;; HYDRA KEYBINDING
(defhydra hydra-window (:hint nil :color pink)
  "
    ^Movement^      ^Split^             ^Switch^     ^Resize^     ^Buffer^       
  ╭───────────────────────────────────────────────────────────────────╯
    [_h_] ←         [_r_] vertical      [_C-h_] ←    [_H_] X←     [_e_] buffer
    [_t_] ↓         [_g_] horizontal    [_C-t_] ↓    [_T_] X↓     [_o_] find
    [_c_] ↑         [_z_] undo          [_C-c_] ↑    [_C_] X↑     [_'_] previous 
    [_n_] →         [_y_] reset         [_C-n_] →    [_N_] X→     [_,_] next
    [_F_] follow    [_d_] delete        ^     ^      [_b_] bal    [_w_] delete
    ^ ^             [_l_] other         
"
  ("h" windmove-left)
  ("t" windmove-down)
  ("c" windmove-up)
  ("n" windmove-right)
  ("F" follow-mode)
  ("r" split-window-right)
  ("g" split-window-below)
  ("z" winner-undo)
  ("y" winner-redo)
  ("d" delete-window)
  ("l" delete-other-windows)
  ("C-h" buf-move-left)
  ("C-t" buf-move-down)
  ("C-c" buf-move-up)
  ("C-n" buf-move-right)
  ("H" shrink-window-horizontally)
  ("T" shrink-window)
  ("C" enlarge-window)
  ("N" enlarge-window-horizontally)
  ("b" balance-windows)
  ("e" helm-mini)
  ("o" helm-find-files)
  ("'" my-previous-user-buffer)
  ("," my-next-user-buffer)
  ("w" kill-this-buffer)
  ("1" my-split-horizontaly :color blue)
  ("2" my-split-verticaly :color blue)
  ("3" split-2-1 :color blue)
  ("4" split-2-2 :color blue)
  ("5" split-3-0 :color blue)
  ("6" split-3-1 :color blue)
  ("7" split-3-3 :color blue)
  ("8" split-2-2-eshell :color blue)
  ("9" split-3-3-3 :color blue)
  ("q" nil))



(defhydra hydra-elscreen (:color red :hint nil)
  "
  [_c_] create    [_n_] next        [_d_] kill     [_e_] helm     [_i_] show-tab
  [_C_] clone     [_h_] previous    [_D_] killB    [_j_] dired    [_b_] show-buf
  [_a_] toggle    [_t_] goto        [_s_] swap     [_l_] list
"
  ("a" elscreen-toggle)
  ("c" elscreen-create)
  ("C" elscreen-clone)
  ("d" elscreen-kill)
  ("D" elscreen-kill-screen-and-buffers)
  ("s" elscreen-swap)
  ("n" elscreen-next)
  ("h" elscreen-previous)
  ("t" elscreen-goto)
  ("e" helm-elscreen :color blue)
  ("j" elscreen-dired)
  ("i" elscreen-toggle-display-tab)
  ("b" elscreen-toggle-display-screen-number)
  ("l" elscreen-display-screen-name-list)
  ("1" (elscreen-goto 0) :color blue)
  ("2" (elscreen-goto 1) :color blue)
  ("3" (elscreen-goto 2) :color blue)
  ("4" (elscreen-goto 3) :color blue)
  ("5" (elscreen-goto 4) :color blue)
  ("g" keyboard-quit)
  ("q" nil :color blue))



(defhydra hydra-yasnippet (:color blue :hint nil)
  "
     ^Modes^   ^Load/Visit^   ^Actions^
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
    [_f_] file        [_a_] grep          [_e_] switch         [_g_] magit
    [_o_] file dwim   [_s_] occur         [_v_] show all       [_p_] project
    [_r_] recent file [_S_] replace       [_V_] ibuffer        [_i_] info
    [_d_] dir         [_t_] find tag      [_K_] kill all
    [_l_] other       [_T_] make tags  
    [_u_] test file     
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
  ("a"   helm-projectile-grep)
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
  ("p"   helm-projectile-switch-project)
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
 ^Align^             ^Sort^              ^Replace^        ^Occur^          ^Grep s/j^     ^Fill Lines^
╭────────────────────────────────────────────────────────────────────────────────────────────╯
 [_aa_] align        [_ol_] lines        [_e_] regexp     [_po_] occur     [_;g_] grep     [_ik_] keep
 [_ac_] current      [_op_] paragraphs   [_E_] replace    [_pm_] multi     [_;l_] lgrep    [_is_] flush
 [_ae_] entire       [_oP_] Pages        [_._] mark       [_pa_] match     [_;r_] rgrep    [_if_] paragrah
 [_ah_] highlight    [_of_] fields       [_\'_] prev     [_pp_] project   [_;d_] delete   [_ir_] region   
 [_an_] new          [_on_] numerics     [_,_] next       [_ph_] helm      [_;j_] join     [_iw_] column
 [_ar_] regex        [_oc_] columns      [_as_] one       [_pt_] helm-m    [_;o_] ins      [_i._] prefix
 [_au_] un-hilight   [_or_] regex        [_an_] all       ^   ^             ^  ^           [_il_] centerL
  ^  ^               [_oR_] reverse      [_at_] cycle     ^   ^             ^  ^           [_ih_] centerR
"
  ("n" forward-char)
  ("h" backward-char)
  ("r" forward-word)
  ("R" my-forward-block)
  ("g" backward-word)
  ("G" my-backward-block)
  ("l" my-select-current-line)
  ("L" my-select-current-block)
  ("t" next-line)
  ("c" previous-line)
  ("T" scroll-up-command)
  ("C" scroll-down-command)
  ("m" org-mark-ring-push)
  ("/" org-mark-ring-goto :color blue)
  ("b" beginning-of-buffer)
  ("B" end-of-buffer)
  ("d" beginning-of-line)
  ("D" end-of-line)
  ("[" backward-sexp)
  ("]" forward-sexp)
  ("j" my-copy-line-or-region)
  ("Q" append-next-kill)
  ("k" yank)
  ("K" yank-pop)
  ("aa" align)
  ("ac" align-current)
  ("ae" align-entire)
  ("ah" align-highlight-rule)
  ("an" align-newline-and-indent)
  ("ar" align-regexp)
  ("au" align-unhighlight-rule)
  ("as" just-one-space)
  ("an" delete-horizontal-space)
  ("at" cycle-spacing)
  ("oP" sort-pages)
  ("oR" reverse-region)
  ("oc" sort-columns)
  ("of" sort-fields)
  ("ol" sort-lines)
  ("on" sort-numeric-fields)
  ("op" sort-paragraphs)
  ("or" sort-regexp-fields)
  ("e" vr/replace :color blue)
  ("E" vr/query-replace :color blue)
  ("." vr/mc-mark :color blue)
  ("po" occur :color blue)
  ("pm" multi-occur :color blue)
  ("pa" multi-occur-in-matching-buffers :color blue)
  ("pp" projectile-multi-occur :color blue)
  ("ph" helm-occur :color blue)
  ("pt" multi-occur-in-matching-buffers :color blue)
  ("\'" smartscan-symbol-go-backward)
  ("," smartscan-symbol-go-forward)
  (";g" grep :color blue)
  (";l" lgrep :color blue)
  (";r" rgrep :color blue)
  (";d" delete-blank-lines)
  (";j" join-line-or-lines-in-region)
  (";o" open-line)
  ("ik" keep-lines :color blue)
  ("is" flush-lines :color blue)
  ("if" fill-paragraph) 
  ("ir" fill-region) 
  ("iw" set-fill-column) 
  ("i." set-fill-prefix) 
  ("il" center-line)
  ("ih" center-region)
  ("ij" join-line-or-lines-in-region)
  ("C-t" my-toogle-case)
  ("x" exchange-point-and-mark)
  ("w" delete-trailing-whitespace)
  ("s" er/expand-region)
  ("!" shell-command-on-region)
  ("u" universal-argument)
  ("z" undo-tree-undo)
  ("y" undo-tree-redo)
  ("SPC" set-mark-command)
  ("C-\'" mc-hide-unmatched-lines-mode)
  ("q" nil :color blue))


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
  ("h" (transparency-increase))
  ("t" (transparency-decrease ))
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
      ^^^^Drag^^^^          ^Transpose^                          ^Org^
╭──────────────────────────────────────────────────────────────────────────╯
       ^^^_c_^^^            [_s_] characters  [_r_] sentences    [_o_] word
       ^^^^↑^^^^            [_w_] words       [_p_] paragraphs   [_e_] elements
_H_  _h_ ←   → _n_ _N_      [_l_] line        [_d_] fix          [_i_] table
       ^^^^↓^^^           ╭─────────────────────┐
       ^^^_t_^^^            [_z_] cancel   [_y_ redo]
"
  ("c" drag-stuff-up)
  ("h" (transpose-sexps -1))
  ("n" transpose-sexps)
  ("H" drag-stuff-left)
  ("N" drag-stuff-right)
  ("t" drag-stuff-down)
  ("s" transpose-chars)
  ("S" (transpose-chars -1))
  ("w" transpose-words)
  ("W" (transpose-words -1))
  ("l" transpose-lines)
  ("L" (transpose-lines -1))
  ("r" transpose-sentences)
  ("R" (transpose-sentences -1))
  ("p" transpose-paragraphs)
  ("P" (transpose-paragraphs -1))
  ("d" transpose-chars :color blue)
  ("o" org-transpose-words)
  ("e" org-transpose-elements)
  ("i" org-table-transpose-table-at-point)
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
        ^^↑^^        [_e_] delete      [_j_] copy     [_r_] reset   [_l_] mark
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
  ("l" vr/mc-mark)
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



(defhydra hydra-multiple-cursors (:hint nil :color pink)
  "
^Up^            ^Down^          ^Multiple^    ^Other^       ^Search^        ^Special^
╭────────────────────────────────────────────────────────────────────────────────────────╯
[_h_] Next      [_n_] Next      [_r_] Line    [_a_] All     [_._] Next       [_in_] numbers
[_H_] Skip      [_N_] Skip      [_l_] Begin   [_d_] Regex   [_,_] Previous   [_il_] letters
[_c_] Unmark    [_t_] Unmark    [_/_] End     [_j_] Copy    [_k_] Paste      [_is_] sort
^ ^             ^ ^             ^ ^           [_e_] del     [_o_] fun        [_ir_] reverse
"
  ("h" my-mc/mark-previous-like-this)
  ("H" mc/skip-to-previous-like-this)
  ("t" mc/unmark-previous-like-this)
  ("n" my-mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("c" mc/unmark-next-like-this)
  ("r" mc/edit-lines :color blue)
  ("l" mc/edit-beginnings-of-lines :color blue)
  ("/" mc/edit-ends-of-lines :color blue)
  ("a" mc/mark-all-like-this :color blue)
  ("o" mc/mark-all-like-this-dwim :color blue)
  ("d" vr/mc-mark :color blue)
  ("e" backward-delete-char-untabify)
  ("." phi-search)
  ("," phi-search-backward)
  ("j" copy-rectangle-as-kill)
  ("k" yank-rectangle)
  ("s" er/expand-region)
  ("in" mc/insert-numbers)
  ("il" mc/insert-letters)
  ("is" mc/sort-regions)
  ("ir" mc/reverse-regions)
  ("z" undo-tree-undo)
  ("y" undo-tree-redo)
  ("q" nil :color blue)
  ("g" mc/keyboard-quit)
  ("\'" mc-hide-unmatched-lines-mode)
  ("-" mc-hide-unmatched-lines-mode)
  ("C-\'" mc-hide-unmatched-lines-mode)
  ("C--" mc-hide-unmatched-lines-mode))



(defhydra hydra-bookmark (:hint nil :color blue)
  "
     ^all^         ^file^       ^desktop^    ^url^  
╭─────────────────────────────────────────────────────╯
     [_a_] jump    [_o_] jump   [_e_] jump   [_u_] jump
     [_d_] delete  [_._] set    [_._] set    [_p_] set
"
  ("a" helm-bookmarks)
  ("d" bookmark-delete)
  ("o" bmkp-autofile-jump)
  ("," bmkp-autofile-set)
  ("e" bmkp-desktop-jump)
  ("." bmkp-set-desktop-bookmark)
  ("u" bmkp-url-jump)
  ("p" bmkp-url-target-set)
  ("q" nil :color blue))


(defhydra hydra-macro (:hint nil :color pink :pre 
                             (when defining-kbd-macro
                                 (kmacro-end-macro 1)))
  "
  ^^Create-Cycle^^   ^Basic^            ^   ^       ^Insert^        ^Save^           ^Edit^
╭───────────────────────────────────────────────────────────────────────────────────╯
     ^_c_^           [_a_] defun      [_m_] step    [_i_] insert    [_b_] name       [_'_] previous
     ^^↑^^           [_o_] edit       [_s_] swap    [_s_] set       [_k_] key        [_,_] last
 _h_ ←   → _n_       [_e_] execute    [_v_] view    [_r_] add       [_x_] register   [_._] loosage
     ^^↓^^           [_d_] delete     ^   ^         [_f_] format                        
     ^_t_^           [_r_] region                                    
    ^^   ^^                
"
  ("h" kmacro-start-macro :color blue)
  ("n" kmacro-end-or-call-macro-repeat)
  ("N" kmacro-end-or-call-macro-repeat :color blue)
  ("c" kmacro-cycle-ring-previous)
  ("t" kmacro-cycle-ring-next)
  ("a" insert-kbd-macro)
  ("r" apply-macro-to-region-lines)
  ("d" kmacro-delete-ring-head)
  ("e" helm-execute-kmacro)
  ("o" kmacro-edit-macro-repeat)
  ("m" kmacro-step-edit-macro)
  ("s" kmacro-swap-ring)
  ("i" kmacro-insert-counter)
  ("l" kmacro-set-counter)
  ("r" kmacro-add-counter)
  ("f" kmacro-set-format)
  ("b" kmacro-name-last-macro)
  ("k" kmacro-bind-to-key)
  ("x" kmacro-to-register)
  ("'" kmacro-edit-macro)
  ("," edit-kbd-macro)
  ("." kmacro-edit-lossage)
  ("u" universal-argument)
  ("z" undo-tree-undo)
  ("y" undo-tree-redo)
  ("v" kmacro-view-macro)
  ("V" kmacro-view-ring-2nd-repeat)
  ("q" nil :color blue))



(defhydra hydra-ggtags (:color teal :hint nil)
  "
 ^Find^                           ^Other^         ^Options^
╭─────────────────────────────────────────────────────────╯
  [_d_] definition  [_e_] dwin    [_-_] repl      v search
  [_f_] file        [_o_] reg     [_z_] def       l navigation
  [_s_] symbol      [_i_] query   [_/_] update    m option
  [_r_] reference   [_'_] prev
  [_c_] continue[   [_,_] next
"
  
  ("d" ggtags-find-definition)
  ("f" ggtags-find-file)
  ("s" ggtags-find-other-symbol)
  ("r" ggtags-find-reference)
  ("c" ggtags-find-tag-continue)
  ("e" ggtags-find-tag-dwim)
  ("o" ggtags-find-tag-regexp)
  ("g" ggtags-grep)
  ("i" ggtags-idutils-query)
  ("," ggtags-next-mark)
  ("'" ggtags-prev-mark)
  ("-" ggtags-query-replace)
  ("z" ggtags-show-definition)
  ("/" ggtags-update-tags)
  ("vv" ggtags-view-search-history)
  ("va" ggtags-view-search-history-action)
  ("vk" ggtags-view-search-history-kill)
  ("vl" ggtags-view-search-history-mode)
  ("vn" ggtags-view-search-history-next)
  ("vh" ggtags-view-search-history-prev)
  ("vu" ggtags-view-search-history-update)
  ("vh" ggtags-view-tag-history)
  ("v." ggtags-view-tag-history-mode)
  ("ln" ggtags-navigation-isearch-forward)
  ("ll" ggtags-navigation-last-error)
  ("l." ggtags-navigation-mode)
  ("lt" ggtags-navigation-next-file)
  ("lc" ggtags-navigation-previous-file)
  ("ls" ggtags-navigation-start-file)
  ("lv" ggtags-navigation-visible-mode)
  ("mb" ggtags-browse-file-as-hypertext)
  ("mc" ggtags-create-tags)
  ("md" ggtags-delete-tags)
  ("me" ggtags-explain-tags)
  ("mk" ggtags-kill-file-buffers)
  ("mw" ggtags-kill-window)
  ("mr" ggtags-reload)
  ("ms" ggtags-save-project-settings)
  ("me" ggtags-save-to-register)
  ("ml" ggtags-toggle-project-read-only)
  ("q" nil :color blue))


(defhydra hydra-major (:color teal :hint nil)
  "
    [_t_] text  [_d_] diff    [_l_] prog     [_o_] org
    [_h_] html  [_c_] css     [_s_] scss     [_j_] jinja
    [_J_] js    [_p_] python  [_C_] clojure  [_r_] ruby  [_e_] elisp
    [_n_] json  [_m_] md      [_x_] jsx
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
  ("x" js2-jsx-mode)
  ("q" nil :color blue)
  ("g" keyboard-quit))



(defhydra hydra-minor (:color amaranth :hint nil)
  "
    [_a_] abbrev    [_d_] debu   [_l_] line     [_n_] nyan     [_wb_] sub
    [_r_] truncate  [_s_] save   [_t_] typo     [_v_] visual   [_ws_] sup
    [_e_] desktop                [_f_] flyspell [_c_] flycheck  [_C-t_] case
     ^ ^             ^ ^         [_p_] fly-prog
"
  ("a" abbrev-mode)
  ("d" toggle-debug-on-error)
  ("f" auto-fill-mode)
  ("l" line-number-mode)
  ("n" nyan-mode)
  ("r" toggle-truncate-lines)
  ("s" auto-save-buffers-enhanced-toggle-activity)
  ("t" typo-mode)
  ("v" visual-line-mode)
  ("e" desktop-save-mode)
  ("f" flyspell-mode)
  ("p" flyspell-prog-mode)
  ("c" flycheck-mode)
  ("wb" subword-mode)
  ("ws" superword-mode)
  ("C-t" my-toogle-case)
  ("q" nil :color blue)
  ("g" nil))



(defhydra hydra-execute (:color blue :hint nil)
  "
[_o_] open  [_e_] execute  [_w_] dirname
"
  ("o" my-open-with)
  ("e" my-sudo)
  ("w" my-dirname-buffer)
  ("r" global-auto-revert-mode)
  ("R" auto-revert-mode)
  ("q" nil :color blue)
  ("g" nil))


(defhydra hydra-apropos (:color teal)
  ("a" helm-apropos "apropos")
  ("w" helm-man-woman "man")
  ("c" apropos-command "cmd")
  ("d" apropos-documentation "doc")
  ("e" apropos-value "val")
  ("l" apropos-library "lib")
  ("o" apropos-user-option "option")
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
  ("q" hide-sublevels)
  ("t" hide-body)
  ("o" hide-other)
  ("r" hide-entry)
  ("l" hide-leaves)
  ("d" hide-subtree)
  ("a" show-all)
  ("e" show-entry)
  ("i" show-children)
  ("k" show-branches)
  ("s" show-subtree)
  ("u" outline-up-heading)
  ("c" outline-previous-visible-heading)
  ("t" outline-next-visible-heading)
  ("n" outline-forward-same-level)
  ("h" outline-backward-same-level)
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
  ("5" markdown-insert-header-atx-5)
  ("6" markdown-insert-header-atx-6)
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
(bind-key "C-n" 'hydra-navigate/body)
(bind-key "C-." 'hydra-execute/body)
(bind-key "C-x ." 'hydra-ggtags/body)
(bind-key "C-x -" 'hydra-yasnippet/body)
(bind-key "C-x p" 'hydra-project/body)
(bind-key "C-x =" 'hydra-adjust/body)
(bind-key "C-x t" 'hydra-transpose/body)
(bind-key* "C-x r" 'hydra-rectangle/body)
(bind-key "C-x d" 'hydra-bookmark/body)
(bind-key "C-x k" 'hydra-macro/body)
(bind-key "C-x K" 'kmacro-end-macro)
(bind-key "C-x a" 'hydra-apropos/body)
(bind-key "C-x M" 'hydra-major/body)
(bind-key "C-x m" 'hydra-minor/body)
(bind-key "C-x v" 'hydra-theme/body)
(bind-key "C-x h" 'hydra-helm/body)
(bind-key "C-x l" 'hydra-desktop/body)


(add-hook 'outline-mode-hook (lambda() (bind-key "C-x x" #'hydra-outline/body outline-mode-map)))
(add-hook 'markdown-mode-hook (lambda() (bind-key "C-x x" #'hydra-markdown/body markdown-mode-map)))
(add-hook 'pdf-view-mode-hook  (lambda() (bind-key "C-x x" #'hydra-pdftools/body pdf-view-mode-map)))



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
 '(blink-cursor-mode nil)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(browse-url-browser-function (quote browse-url-chromium))
 '(column-number-mode t)
 '(company-backends
   (quote
    (company-tern company-go company-ycmd company-racer company-c-headers company-robe company-css company-semantic company-xcode company-cmake company-dabbrev-code company-capf company-gtags company-files)))
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("cd560f7570de0dcdcf06953b3f1a25145492a54f100f9c8da3b4091b469f7f02" "b9293d120377ede424a1af1e564ba69aafa85e0e9fd19cf89b4e15f8ee42a8bb" "38e64ea9b3a5e512ae9547063ee491c20bd717fe59d9c12219a0b1050b439cdd" "cf28bfffbf8726a31989e662986065b5319670902ac1af0e63fb8e773c119488" "6df30cfb75df80e5808ac1557d5cc728746c8dbc9bc726de35b15180fa6e0ad9" "f64c9f8b4241b680b186f4620afb9c82fa2a76cf4498a7431f90db59bb1892eb" "34e7163479ef3669943b3b9b1fabe639d6e0a0453e0de79cea2c52cb520d3bc4" "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132" "98a619757483dc6614c266107ab6b19d315f93267e535ec89b7af3d62fb83cad" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" "68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "8aa7eb0cc23931423f719e8b03eb14c4f61aa491e5377073d6a55cba6a7bc125" "0fb6369323495c40b31820ec59167ac4c40773c3b952c264dd8651a3b704f6b5" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "196cc00960232cfc7e74f4e95a94a5977cb16fd28ba7282195338f68c84058ec" "dcf229d4673483cb7b38505360824fa56a0d7b52f54edbcdca98cf5059fa1662" "067d9b8104c0a98c916d524b47045367bdcd9cf6cda393c5dae8cd8f7eb18e2a" "0820d191ae80dcadc1802b3499f84c07a09803f2cb90b343678bdb03d225b26b" "94ba29363bfb7e06105f68d72b268f85981f7fba2ddef89331660033101eb5e5" "cdd26fa6a8c6706c9009db659d2dffd7f4b0350f9cc94e5df657fa295fffec71" "47ac4658d9e085ace37e7d967ea1c7d5f3dfeb2f720e5dec420034118ba84e17" "af960831c1b33b719cda2ace858641dd8accc14d51e8ffb65b39ca75f07d595d" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "8fed5e4b89cf69107d524c4b91b4a4c35bcf1b3563d5f306608f0c48f580fdf8" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "3ed645b3c08080a43a2a15e5768b893c27f6a02ca3282576e3bc09f3d9fa3aaa" "f0d8af755039aa25cd0792ace9002ba885fd14ac8e8807388ab00ec84c9497d7" "11636897679ca534f0dec6f5e3cb12f28bf217a527755f6b9e744bd240ed47e1" "50ce37723ff2abc0b0b05741864ae9bd22c17cdb469cae134973ad46c7e48044" "08851585c86abcf44bb1232bced2ae13bc9f6323aeda71adfa3791d6e7fea2b6" "01d299b1b3f88e8b83e975484177f89d47b6b3763dfa3297dc44005cd1c9a3bc" "c3c0a3702e1d6c0373a0f6a557788dfd49ec9e66e753fb24493579859c8e95ab")))
 '(delete-selection-mode 1)
 '(exec-path
   (append exec-path
           (quote
            ("/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/sbin" "/bin" "/opt/node/bin"))))
 '(fci-rule-color "#383838")
 '(inf-ruby-default-implementation "ruby")
 '(initial-major-mode (quote org-mode))
 '(initial-scratch-message nil)
 '(irony-additional-clang-options (quote ("-std=c++11")))
 '(js-indent-level 2)
 '(org-babel-python-command "python3")
 '(package-selected-packages
   (quote
    (elm-mode yafolding php-extras php-mode-map company-php unicode-fonts buffer-move neotree cider-mode cider popwin elisp--witness--lisp company-irony expand-region company-quickhelp company yaml-mode windata use-package tree-mode smartparens shm scss-mode rainbow-delimiters python-info pydoc-info nyan-mode multiple-cursors molokai-theme markdown-mode lua-mode leuven-theme json-rpc json-mode js2-mode jinja2-mode jedi iedit hi2 helm-swoop helm-projectile helm-hoogle helm-css-scss helm-company goto-chg fullscreen-mode framemove f emmet-mode drag-stuff company-tern company-jedi coffee-mode auto-save-buffers-enhanced auto-compile)))
 '(prefer-coding-system (quote utf-8))
 '(ring-bell-function (quote ignore) t)
 '(same-window-buffer-names (quote ("*shell*")))
 '(scroll-error-top-bottom t)
 '(typescript-indent-level 4)
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


;; theme and font
(set-default-font "DejaVu Sans Mono 9")

(load-theme 'grandshell)
;; (load-theme 'monokai)
;; (load-theme 'cyberpunk)
;; (load-theme 'assemblage)
;; (load-theme 'sanityinc-tomorrow-night)


;; mac related 
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
