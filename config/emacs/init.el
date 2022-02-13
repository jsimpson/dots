;;; init.el

;; who-am-i?
; used by git, mail, etc.
(setq user-mail-address "jsimpson.github@gmail.com")
(setq user-full-name "Jonathan Simpson")

;; spell check - install "aspell" and aspell dictionaries.
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra"))

;; UI customization
(menu-bar-mode 0)
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))
(column-number-mode 't)
(global-visual-line-mode 1)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(dolist (mode '(term-mode-hook shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Hack" :height 125)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; load custom dracula pro / doom dracula pro themes
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")

;; show stray whitespace
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; consider a period followed by a single space the end of sentence
(setq sentence-end-double-space nil)

;; use spaces, not tabs, for indentation
(setq-default indent-tabs-mode nil)

;; display the distance between two tab stops as 4 characters wide
(setq-default tab-width 4)

;; highlight matching pairs of parentheses
(setq show-paren-delay 0)
(show-paren-mode)

;; write auto-saves and backups to separate directory
(make-directory "~/.config/emacs/backup/" t)
(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/backup/" t)))
(setq backup-directory-alist '(("." . "~/.config/emacs/backup/margo/")))

;; do not move the current file while creating backup
(setq backup-by-copying t)

;; disable lockfiles
(setq create-lockfiles nil)

;; load package manager, add the MELP package repository
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(dolist (mode '(treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package doom-themes
  :init (load-theme 'doom-dracula-pro t)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer jsi/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (jsi/leader-key-def
    "t" '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "ts" '(hydra-text-scale/body :which-key "zoom"))
  (general-define-key
   "C-c l" #'org-store-link
   "C-c a" #'org-agenda
   "C-c c" #'org-capture)
  (general-define-key
   "C-s" 'swiper
   "M-x" 'counsel-M-x)
  (general-define-key
   :prefix "C-c"
   "b" 'ivy-switch-buffer
   "/" 'counsel-git-grep
   "f" '(:ignore t :which-key "files")
   "ff" 'counsel-find-file
   "fr" 'counsel-recentf
   "fr" 'counsel-recentf
   "p" '(:ignore t :which-key "project")
   "pf" '(counsel-git :which-key "find file in git dir")))

(use-package which-key
  :init
  (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package slime
  :init
  (setq inferior-lisp-program "sbcl"))

; language servers
(setq gc-cons-threshold 100000000) ; speed up initialization, reducing GC runs
(setq read-process-output-max (* 1024 1024 3)) ; 3mb
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :config (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

; languages
(use-package go-mode)
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook (lambda ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)))

(use-package csharp-mode)
(add-hook 'csharp-mode-hook #'lsp-deferred)

(use-package dotnet)
(add-hook 'csharp-mode-hook 'dotnet-mode)

(use-package csproj-mode)

; completions
(use-package company
  :after (lsp-mode)
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map ("<tab>" . company-complete-selection))
  (:map lsp-mode-map ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

; treemacs / file navigation
(use-package treemacs
  :defer t
  :bind
  (:map global-map ([f8] . treemacs))
  (:map global-map ("C-<f8>" . treemacs-select-window))
  :config
  (setq treemacs-is-never-other-window t))
(use-package treemacs-evil
  :after (treemacs evil))
(use-package treemacs-projectile
  :after (treemacs projectile))
(use-package lsp-treemacs
  :after (treemacs lsp))

(use-package ivy
  :init (ivy-mode))
(use-package swiper)
(use-package counsel
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-variable] . counsel-describe-variable))
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; load evil
(use-package evil
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ; use visual line mode even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package org
  :config
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  :custom
  (org-agenda-files '("~/notes/work/tasks.org"
                      "~/notes/personal/dates.org")))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package ob-go)

(org-babel-do-load-languages
 'org-babel-load-languages '((emacs-lisp . t)
                             (C . t)
                             (go . t)
                             (python . t)
                             (ruby . t)
                             (shell . t)
                             (sql . t)
                             (sqlite . t)))

(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "zoom"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom (projectile-completion-system 'ivy)
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("~/notes/" "~/projects/" "~/work/"))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; load paredit
(use-package paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)

;; load rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; load markdown mode
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; load magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; load flycheck
(use-package flycheck
  :init (global-flycheck-mode))

;; load helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

;; M-x all-the-icons-install-fonts must be run interactively on a new machine after installing the package.
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; start server
(require 'server)
(unless (server-running-p)
  (server-start))

;;; .emacs.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(org-agenda-files '("~/notes/work/tasks.org" "~/notes/personal/dates.org") nil nil "Customized with use-package org")
 '(package-selected-packages
   '(go-mode org-bullets doom-themes evil-magit hydra evil-collection doom-modeline powerline all-the-icons helpful ivy-rich yasnippet which-key Use-package slime rainbow-delimiters paredit magit lsp-mode ido-vertical-mode helm-projectile go-projectile general flycheck evil dracula-theme counsel company avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
