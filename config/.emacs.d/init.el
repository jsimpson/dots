;;; .Emacs

;; who-am-i?
; used by git, mail, etc.
(setq user-mail-address "jsimpson.github@gmail.com")
(setq user-full-name "Jonathan Simpson")

;; spell check - install "aspell" and aspell dictionaries.
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra"))

;; Customize user interface.
(menu-bar-mode 0)
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))
;(setq inhibit-startup-screen t)
(column-number-mode 't)
(global-visual-line-mode 1)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(dolist (mode '(term-mode-hook
                shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; load custom dracula pro theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula-pro t)

;; interactive mode everywhere
;(ido-mode 1)
;(ido-everywhere)
;(setq ido-enable-flex-matching t)
;(fido-mode)

;; show stray whitespace
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; consider a period followed by a single space the end of sentence
(setq sentence-end-double-space nil)

;; sse spaces, not tabs, for indentation
(setq-default indent-tabs-mode nil)

;; display the distance between two tab stops as 4 characters wide
(setq-default tab-width 4)

;; highlight matching pairs of parentheses
(setq show-paren-delay 0)
(show-paren-mode)

;; write auto-saves and backups to separate directory
(make-directory "~/.emacs.d/backup/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/backup/" t)))
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/margo/")))

;; do not move the current file while creating backup
(setq backup-by-copying t)

;; disable lockfiles
(setq create-lockfiles nil)

;; load package manager, add the MELP package repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package general
  :config
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
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode))

;; load paredit
(use-package paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)

;; load rainbow delimiters
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#80ffea") ; cyan
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#8aff80") ; green
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#ffca80") ; orange
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#ff80bf") ; pink
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#9580ff") ; purple
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#ff9580") ; red
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#ffff80")) ; yellow)

;; load markdown mode
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; load magit
(use-package magit
  :init (setq magit-fetch-modules-jobs 16))

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

;; start server
(require 'server)
(unless (server-running-p)
  (server-start))

;;; .emacs.el ends here
