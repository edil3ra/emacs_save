(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)



(setq package-check-signature nil)
(package-initialize)

(when (not package-archive-contents)
   (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(straight-use-package 'el-patch)
(setq use-package-always-ensure nil)

(defun jjin/use-package-if-prehook (name _keyword pred rest state)
  (unless pred (error "predicated failed; skipping package")))
(advice-add 'use-package-handler/:if :before 'jjin/use-package-if-prehook)

(use-package system-packages
  :init
  (setq system-packages-use-sudo nil)
  (when (eq system-type 'darwin)
    (setq system-packages-package-manager 'brew)))




;; (straight-use-package 'org) ; or org-plus-contrib if desired


(org-babel-load-file (expand-file-name "~/.emacs.d/settings.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(beacon-color "#d54e53")
 '(blink-cursor-mode nil)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(current-language-environment "UTF-8")
 '(custom-safe-themes
   '("6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "356e5cbe0874b444263f3e1f9fffd4ae4c82c1b07fe085ba26e2a6d332db34dd" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "e9d47d6d41e42a8313c81995a60b2af6588e9f01a1cf19ca42669a7ffd5c2fde" "5adf7ad078568675387aac96e142c1300006531721bca35b941e4ed3e3b59000" "84106b6b1ea9fe100a88ca60ee0a334fa20b8f80f6e81c362a0c0f709b01f2c2" "1a232652b04b68380b1cff7ceeb62787b4eb43df826a97c67831c50b0c0d1451" default))
 '(dap-lldb-debug-program
   '("/home/vince/.vscode-oss/extensions/lanza.lldb-vscode-0.1.0/bin/darwin/bin/lldb"))
 '(dap-mode t nil (dap-mode))
 '(dap-ui-mode t nil (dap-ui))
 '(delete-selection-mode t)
 '(dired-sidebar-icon-scale 0.5)
 '(dired-sidebar-theme "nerd")
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };"))
 '(exec-path
   (append exec-path
           '("/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/sbin" "/bin" "/opt/node/bin")))
 '(expand-region-contract-fast-key "")
 '(fci-rule-color "#383838")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(flycheck-display-errors-function nil)
 '(gdb-display-io-nopopup t)
 '(gnus-logo-colors '("#2fdbde" "#c0c0c0") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };") t)
 '(helm-completion-style 'emacs)
 '(highlight-indent-guides-auto-even-face-perc 15)
 '(highlight-indent-guides-auto-odd-face-perc 15)
 '(highlight-indent-guides-method 'column)
 '(hl-sexp-background-color "#efebe9")
 '(initial-major-mode 'org-mode)
 '(irony-additional-clang-options '("-std=c++11"))
 '(linum-format " %6d ")
 '(main-line-color1 "#222912")
 '(main-line-color2 "#09150F")
 '(package-selected-packages
   '(ts-comint helm-tramp python-pytest pyhton-pytest pytest forge eglot lsp ob-http vscode-icon all-the-icons dired-sidebar md4rd eslint-fix web-beautify broadcast lsp-ruby magit-find-file git-timemachine ob-async stylus-mode bongo kubernetes docker-compose-mode docker-compose yapfify yapify lsp-javascript-flow lsp-javascript-typescript lsp-typescript lsp-ui company-lsp lsp-flycheck lsp-vue lsp-mode google-translate eredis realgud vagrant-tramp nginx-mode ansible ansible-doc aws-ec2 helm-aws es-mode ob-restclient company-restclient restclient rjsx-mode ng2-mode exsqlaim-mode clues-theme color-theme-sanityinc-tomorrow alect-themes format-sql dired-plus dired dired+ dired-p diredp dired-mode google-this cyberpunk-theme pdf-tools cider-hydra borland-blue-theme flycheck-rust coconut coconut-mode ox-ipynb ox-ipython ob-ipython ein ensime scala-mode ptyhon-docstring-mode ptyhon-docstring pygen python-docstring python-docstring-mode sphinx-doc dockerfile-mode prettier-js xref-js2 dired-rainbow highlight-symbol jump-tree undo-tree ereader eyebrowse elm-mode yafolding php-extras php-mode-map company-php unicode-fonts buffer-move neotree cider-mode cider popwin elisp--witness--lisp company-irony company-quickhelp company yaml-mode windata use-package tree-mode shm scss-mode rainbow-delimiters pydoc-info nyan-mode multiple-cursors molokai-theme markdown-mode lua-mode leuven-theme json-rpc json-mode js2-mode jinja2-mode jedi iedit hi2 helm-projectile helm-hoogle helm-css-scss helm-company goto-chg fullscreen-mode framemove f emmet-mode drag-stuff company-tern company-jedi coffee-mode auto-save-buffers-enhanced auto-compile))
 '(popwin:special-display-config
   '(help-mode
     ("*Miniedit Help*" :noselect t)
     (completion-list-mode :noselect t)
     (compilation-mode :noselect t)
     (grep-mode :noselect t)
     (occur-mode :noselect t)
     ("*Pp Macroexpand Output*" :noselect t)
     "*Shell Command Output*" "*vc-diff*" "*vc-change-log*"
     (" *undo-tree*" :width 60 :position right)
     ("^\\*anything.*\\*$" :regexp t)
     "*slime-apropos*" "*slime-macroexpansion*" "*slime-description*"
     ("*slime-compilation*" :noselect t)
     "*slime-xref*"
     (sldb-mode :stick t)
     slime-repl-mode slime-connection-list-mode))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(prefer-coding-system 'utf-8)
 '(safe-local-variable-values
   '((encoding . utf8)
     (eval flycheck-select-checker 'python-pylint)
     (nil)
     (nxml-attribute-indent . 4)
     (nxml-child-indent . 4)
     (nxml-child-indent . 2)
     (flycheck-select-checker . "python-pylint")
     (web-mode-engines-alist
      ("django" . "\\.html\\'"))))
 '(same-window-buffer-names '("*shell*"))
 '(semantic-mode t)
 '(sp-highlight-pair-overlay t)
 '(sp-highlight-wrap-overlay t)
 '(sp-show-pair-from-inside t)
 '(treemacs-collapse-dirs 3)
 '(treemacs-recenter-after-project-expand 'on-distance)
 '(treemacs-tag-follow-cleanup nil)
 '(treemacs-tag-follow-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#bf616a")
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
     (360 . "#B4EB89")))
 '(vc-annotate-very-old-color nil)
 '(vue-html-extra-indent 4)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elscreen-tab-background-face ((t (:background "green"))))
 '(elscreen-tab-control-face ((t (:background "white" :foreground "green" :underline "green"))))
 '(elscreen-tab-other-screen-face ((t (:background "green" :foreground "green"))))
 '(persp-selected-face ((t (:foreground "green" :weight bold))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "magenta"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "gold"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "violet"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "DarkOrange"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "magenta")))))
(put 'downcase-region 'disabled nil)
(put 'magit-edit-line-commit 'disabled nil)
(put 'upcase-region 'disabled nil)
