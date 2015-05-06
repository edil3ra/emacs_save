(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(require 'ergoemacs-mode)
(require 'multiple-cursors)
(require 'auto-complete-config)
(require 'yasnippet)
(require 'fullscreen-mode)
(require 'ido)
(require 'python)
(require 'virtualenvwrapper)
(require 'nose)
(require 'emmet-mode)
(require 'expand-region)
(require 'bookmark)
(require 'auto-save-buffers-enhanced)
(require 'drag-stuff)
(require 'ace-jump-mode)
(require 'uniquify) 
(require 'framemove)
(require 'nodejs-repl)
(require 'coffee-mode)
(require 'direx)
(require 'flx-ido)
(require 'projectile)
(require 'helm)
(require 'helm-swoop)
(require 'rvm)


;; do default config for auto-complete
(ac-config-default)

;; start yasnippet with emacs
(yas-global-mode 1)

;; ido, ibuffer, smartparens , flex
(ido-mode t)
(defalias 'list-buffers 'ibuffer)
(smartparens-global-mode t)
(ido-mode 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; helm
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)
(helm-mode 1)

;; projectile
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; ace jump mode
(add-to-list 'load-path "/full/path/where/ace-jump-mode.el/in/")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

;; emmet 
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(eval-after-load "emmet-mode"
  '(define-key emmet-mode-keymap (kbd "C-j") nil))


;; framemove
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

;; uniquify
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")


;; PYTHON
;; jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
;; python env
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(setq venv-location "/home/vince/Envs")


;; JAVA
(setq eclim-auto-save t)
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

(add-hook 'java-mode-hook
      (lambda ()
        (local-set-key (kbd "C-c C-c") 'eclim-project-build)
        (local-set-key (kbd "C-<tab>") 'eclim-complete)
        (local-set-key (kbd "C-c c") 'eclim-project-build)
        (local-set-key (kbd "C-c o") 'eclim-project-open)
        (local-set-key (kbd "C-c g") 'eclim-project-goto)
        (local-set-key (kbd "C-c r") 'eclim-java-find-references)
        (local-set-key (kbd "C-c d") 'eclim-java-find-declaration)
        (local-set-key (kbd "C-c h") 'eclim-java-show-documentation-for-current-element)
        (local-set-key (kbd "C-d") 'mc/mark-next-lines)
	  ))


;; JAVASCRIPT
;; js2 mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js-mode-hook 'js2-minor-mode)
(setq ac-js2-evaluate-calls t)

;; tern js
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup))
)
(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

;; COFFEESCRIPT
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2)
  (setq coffee-tab-width 2))

(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

(add-to-list 'ac-modes 'coffee-mode) ;; coffee-modeでACを使えるようにする
(add-hook 'coffee-mode-hook
  '(lambda ()
    (add-to-list 'ac-dictionary-files "~/.emacs.d/elisp/el-get/auto-complete/dict/js2-mode")
))

(add-hook 'coffee-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))


;; RUBY
(add-hook 'enh-ruby-mode-hook 'auto-complete-mode)
(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
(autoload 'robe-mode "robe" "Code navigation, documentation lookup and completion for Ruby" t nil)
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(autoload 'ac-robe-setup "ac-robe" "auto-complete robe" nil nil)
(add-hook 'robe-mode-hook 'ac-robe-setup)

(eval-after-load "ruby-mode"
  '(add-hook 'ruby-mode-hook 'ruby-electric-mode))
(add-hook 'enh-ruby-mode-hook
      (lambda ()
        (local-set-key (kbd "C-c C-c") 'ruby-send-block)))
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session


;; C C++
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)

(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/usr/include$")
)

;; turn on Semantic
;; let's define a function which adds semantic as a suggestion backend to auto complete
;; and hook this function to c-mode-common-hook

;; (defun my:add-semantic-to-autocomplete() 
;;   (semantic-mode 1)
;;   (add-to-list 'ac-sources 'ac-source-semantic)
;; )
;; (add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)

;; turn on ede mode 
(global-ede-mode 1)

;; LUA
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


;; bookmark startup
(setq inhibit-splash-screen t)
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")
(toggle-diredp-find-file-reuse-dir 1)


;; place the ~ file inside a .save directory
(setq backup-directory-alist `(("." . "~/.saves")))
;; save-buffer
(auto-save-buffers-enhanced t)
;; ignore buffer
(setq ido-ignore-buffers '("\\` " "^\*"))

;; ido buffer config
(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("dired" (mode . dired-mode))
	       ("html" (or
			(mode . html-mode)
			(mode . web-mode)
			(mode . jinja2-mode)))
	       ("coffee" (mode . coffee-mode))
	       ("js" (mode . js2-mode))
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

;; save on lost focus to change when i switch version
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


;; ipython as default interpreter for python
(setq
 python-shell-interpreter "ipython3"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
;;add colors to shell
(setq ansi-term-color-vector [unspecified "#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#DD6600" "#dc8cc3" "#93e0e3" "#dcdccc"])


;; run current file
(defun xah-run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file xx.py,
then it'll call “python xx.py” in a shell.
The file can be php, perl, python, ruby, javascript, bash, ocaml, vb, elisp.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first.

If the file is emacs lisp, run the byte compiled version if exist."
  (interactive)
  (let* (
         (suffixMap
          `(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python3")
            ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
            ("rb" . "ruby")
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
	(save-buffer) ) )

    (if (string-equal fSuffix "el") ; special case for emacs lisp
        (load (file-name-sans-extension fName))
      (if progName
          (progn
            (message "Running…")
            (shell-command cmdStr "*xah-run-current-file output*" )
            )
        (message "No recognized program file suffix for this file.")
        ))))


;; run main.py
(defun run_main_py ()
  (interactive)
  (shell-command "python3 main.py"))

;; run ruby
(defun run_main_rb ()
  (interactive)
  (shell-command "ruby main.rb"))


;; find file in dired mod
(define-key dired-mode-map "c" 'find-file)

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
			  '(lambda (a b) (string< (buffer-name a) (buffer-name b)))))))
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
  (command-execute 'balance-windows)
)


;; org language
(org-babel-do-load-languages
'org-babel-load-languages
 '((latex . t)))

(defun org-export-latex-format-toc-org-article (depth)
  (when depth
    (format "\\setcounter{secnumdepth}{%s}\n\\tableofcontents\n"
            depth)))
(setq org-export-latex-format-toc-function 'org-export-latex-format-toc-org-article)





;; dired+ 
(toggle-diredp-find-file-reuse-dir 1)




;; my white favorite theme
;; (load-theme 'leuven t)
(load-theme 'molokai t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(auto-save-default nil)
 '(auto-save-interval 0)
 '(blink-cursor-mode nil)
 '(browse-url-browser-function (quote browse-url-chromium))
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
	("90edd91338ebfdfcd52ecd4025f1c7f731aced4c9c49ed28cfbebb3a3654840b" "08851585c86abcf44bb1232bced2ae13bc9f6323aeda71adfa3791d6e7fea2b6" "bf7fc6225da1e89b364f72258047beb8867ab648518f078e81b68804098360a9" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "01d299b1b3f88e8b83e975484177f89d47b6b3763dfa3297dc44005cd1c9a3bc" "df965b0257f8bb59dff39ce50f540914f62caebffee0310d38640d1af5b68d8b" "df40b58b2e2ceb99893c1beacf472a7b995790e49f9c7ae4b855a1ba0256889e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "a041a61c0387c57bb65150f002862ebcfe41135a3e3425268de24200b82d6ec9" "9a60d8bf511d915a08aa16f97bf2c8b11d55a54ef32118424db7d73d9d7d0401" "7e7968790fda91cc7e03b93eb77b5604752e609a02fb94538dce400552a4c26a")))
 '(custom-theme-load-path (quote (custom-theme-directory t)))
 '(delete-selection-mode t)
 '(dired-clean-up-buffers-too nil)
 '(dired-use-ls-dired t)
 '(drag-stuff-global-mode t)
 '(eclim-ant-directory "")
 '(eclim-auto-save nil)
 '(eclim-eclipse-dirs (quote ("/opt/eclipse")))
 '(eclim-executable "/opt/eclipse/eclim")
 '(electric-indent-mode nil)
 '(emmet-indentation 2)
 '(emmet-preview-default nil)
 '(enh-ruby-hanging-brace-deep-indent-level 2)
 '(enh-ruby-hanging-paren-deep-indent-level 2)
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
 '(erc-track-mode t)
 '(ergoemacs-handle-ctl-c-or-ctl-x (quote only-C-c-and-C-x))
 '(ergoemacs-keyboard-layout "dv")
 '(ergoemacs-mode t)
 '(ergoemacs-theme-options (quote ((fn-keys off))))
 '(ergoemacs-theme-version (quote (("standard" nil))))
 '(expand-region-preferred-python-mode (quote fgallina-python))
 '(fci-rule-color "#343d46")
 '(fullscreen-mode t)
 '(global-eclim-mode t)
 '(global-highlight-changes-mode nil)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-google-suggest-use-curl-p t)
 '(helm-match-plugin-mode t nil (helm-match-plugin))
 '(helm-mode nil)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-scroll-amount 8)
 '(helm-split-window-in-side-p t)
 '(highlight-changes-face-list
   (quote
	(highlight-changes-1 highlight-changes-2 highlight-changes-3 highlight-changes-4 highlight-changes-5 highlight-changes-6)))
 '(imenu-auto-rescan t)
 '(imenup-ignore-comments-flag nil)
 '(imenup-sort-ignores-case-flag nil)
 '(inf-ruby-implementations
   (quote
	(("ruby" . "irb --prompt default --noreadline -r irb/completion")
	 ("jruby" . "jruby -S irb --prompt default --noreadline -r irb/completion")
	 ("rubinius" . "rbx -r irb/completion")
	 ("yarv" . "irb2.2.0 -r irb/completion")
	 ("macruby" . "macirb -r irb/completion")
	 ("pry" . "pry"))))
 '(initial-scratch-message
   #(";; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with ▤ r,
;; then enter the text in that file's own buffer." 131 132
													   (face ergoemacs-pretty-key)
													   133 134
													   (face ergoemacs-pretty-key)))
 '(jedi-direx:hide-imports t)
 '(jedi:complete-on-dot nil t)
 '(jedi:get-in-function-call-delay 1000)
 '(jedi:install-python-jedi-dev-command
   (quote
	("pip3" "install" "--upgrade" "git+https://github.com/davidhalter/jedi.git@dev#egg=jedi")))
 '(jedi:key-complete (quote tab))
 '(jedi:tooltip-method nil)
 '(lua-indent-level 2)
 '(lua-prefix-key "C-c")
 '(markdown-xhtml-standalone-regexp "")
 '(mc/mode-line nil)
 '(menu-bar-mode nil)
 '(mu4e-drafts-folder "/Drafts" t)
 '(mu4e-get-mail-command "offlineimap" t)
 '(mu4e-refile-folder "/Archive" t)
 '(mu4e-sent-folder "/Sent" t)
 '(mu4e-trash-folder "/Trash" t)
 '(mu4e-update-interval 300)
 '(mu4e-user-mail-address-list (quote ("lemarsupu@hotmail.com")))
 '(org-CUA-compatible nil)
 '(org-babel-load-languages (quote ((python . t) (latex . t) (sh . t))))
 '(org-pretty-entities t)
 '(org-pretty-entities-include-sub-superscripts t)
 '(org-replace-disputed-keys nil)
 '(projectile-global-mode t)
 '(python-shell-interpreter "ipython3")
 '(python-shell-interpreter-args "")
 '(python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
 '(python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
 '(python-shell-virtualenv-path nil)
 '(pyvenv-activate ~/Envs)
 '(pyvenv-mode t)
 '(pyvenv-virtualenvwrapper-python "/usr/bin/python3")
 '(rcirc-default-nick "duckhack")
 '(rcirc-default-user-name "lemarsupu")
 '(recentf-menu-before nil)
 '(recentf-mode t)
 '(rsense-temp-file "/tmp/17111bWv")
 '(scroll-all-mode nil)
 '(scroll-bar-mode nil)
 '(scss-compile-at-save nil)
 '(semantic-mode t)
 '(shift-select-mode nil)
 '(show-smartparens-global-mode t)
 '(smex-prompt-string
   #("Alt+A " 0 3
	 (face ergoemacs-pretty-key)
	 4 5
	 (face ergoemacs-pretty-key)))
 '(tool-bar-mode nil)
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
 '(vc-annotate-very-old-color nil)
 '(venv-location "/home/ubuntu/Envs"))

;; theme and font
(enable-theme 'molokai)
(set-default-font "DejaVu Sans Mono 9")



(defun smart-ret()
  (interactive)
  (end-of-line)
  (newline-and-indent))


;; custom global key
(global-set-key (kbd "M--") 'comment-dwim)
(global-set-key (kbd "M-a") 'helm-M-x)

;; (global-set-key (kbd "C-c C-c M-a") 'execute-extended-command)
(global-set-key (kbd "M-1") 'shell-dwim)
(global-set-key (kbd "M-m") 'emmet-expand-line) ; expand current pane
(global-set-key (kbd "C-<tab>") 'ac-complete)
(global-set-key (kbd "S-<tab>") 'ac-complete)

;; action key
;; (global-set-key (kbd "<f1>") 'direx:jump-to-directory)
(global-set-key (kbd "<f1>") 'shell_buffer)
(global-set-key (kbd "<f2>") 'run_main_py)
(global-set-key (kbd "<f5>") 'xah-run-current-file)
(global-set-key (kbd "<f6>") 'recentf-open-files)
(global-set-key (kbd "<f7>") 'helm-bookmarks)
(define-key key-translation-map (kbd "<f8>") (kbd "<menu>"))
(global-set-key (kbd "<f12>") 'fullscreen-mode-fullscreen-toggle) 

;; buffer switching enancement
(global-set-key (kbd "M-'") 'ergoemacs-previous-user-buffer)
(global-set-key (kbd "M-,") 'ergoemacs-next-user-buffer)
(global-set-key (kbd "M-<") 'register-to-point)
(global-set-key (kbd "M-\"") 'point-to-register)

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-e") 'ido-switch-buffer)
(global-set-key (kbd "C-S-e") 'ibuffer)
(global-set-key (kbd "M-w") 'yas-expand)

;; multiple cursors
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-d") 'mc/mark-all-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click) ;

;; helm
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "M-.") 'helm-find-files-up-one-level)
(define-key helm-map (kbd "C-d") 'helm-dired-action)

(global-set-key (kbd "C-p") 'helm-imenu)
(global-set-key (kbd "C-o") 'helm-find-files)
(global-set-key (kbd "M-o") 'helm-projectile-find-file)
(global-set-key (kbd "C-e") 'helm-buffers-list)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-f") 'helm-locate)
(global-set-key (kbd "M-S-f") 'helm-google-suggest)

;; helm swoop
(global-set-key (kbd "C-r") 'helm-swoop)
(global-set-key (kbd "C-S-r") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "M-7") 'helm-multi-swoop)
(global-set-key (kbd "M-8") 'helm-multi-swoop-all)

;; other remappingc
(global-set-key (kbd "M-l") 'ergoemacs-select-current-line)
(global-set-key (kbd "M-L") 'ergoemacs-select-current-block)
(global-set-key (kbd "M-9") 'recenter-top-bottom)
(global-set-key (kbd "M-S") 'er/mark-inside-pairs)
(global-set-key (kbd "M-s") 'er/expand-region)
(global-set-key (kbd "M-RET") 'smart-ret)

;; window switch
(global-set-key (kbd "C-S-h") 'windmove-left)
(global-set-key (kbd "C-S-n") 'windmove-right)
(global-set-key (kbd "C-S-c") 'windmove-up)
(global-set-key (kbd "C-S-t") 'windmove-down)
(global-set-key (kbd "C-#") 'balance-windows)
(global-set-key (kbd "C-$") 'split-3-windows-horizontally-evenly)
(global-set-key (kbd "C-b")  'make-frame-command)


(global-set-key (kbd "C-<left>")  'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "C-<up>")    'windmove-up)
(global-set-key (kbd "C-<down>")  'windmove-down)

(global-set-key (kbd "S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-<down>") 'shrink-window)
(global-set-key (kbd "S-<up>") 'enlarge-window)

;; drag stuff
(global-set-key (kbd "<M-up>") 'drag-stuff-up)
(global-set-key (kbd "<M-down>") 'drag-stuff-down)
(global-set-key (kbd "<M-left>") 'drag-stuff-left)
(global-set-key (kbd "<M-right>") 'drag-stuff-right)
(define-key dired-mode-map (kbd "<f1>") nil)



(add-hook 'js2-mode-hook
      (lambda ()
        (local-set-key (kbd "<C-tab>") 'tern-ac-complete)))


(add-hook 'coffee-mode-hook
      (lambda ()
        (local-set-key (kbd "<C-tab>") 'tern-ac-complete)))


(add-hook 'coffee-mode-hook
      (lambda ()
        (local-unset-key (kbd "<M-i>"))))

;; org mode unbind
(add-hook 'org-mode-hook
      (lambda ()
        (local-unset-key (kbd "M-a"))))

;; cc mode
(add-hook 'c++-mode-hook
      (lambda ()
        (local-unset-key (kbd "M-a"))))

;; c mode
(add-hook 'c-mode-hook
      (lambda ()
        (local-unset-key (kbd "M-a"))))

;; java mode
(add-hook 'java-mode-hook
      (lambda ()
        (local-unset-key (kbd "M-a"))
		))


;; dir mode+ unbind
(add-hook 'dired-mode-hook
      (lambda ()
        (local-set-key (kbd "M-s") 'er/expand-region)))

(add-hook 'dired-mode-hook
      (lambda ()
        (local-unset-key (kbd "C-o"))))

(add-hook 'dired-mode-hook
      (lambda ()
        (local-set-key (kbd "M-g") 'backward-word)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
