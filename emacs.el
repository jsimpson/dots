;;; .emacs

;; who-am-i?
; used by git, mail, etc.
(setq user-mail-address "jsimpson.github@gmail.com")
(setq user-full-name "Jonathan Simpson")

;; Spell check - install "aspell" and aspell dictionaries.
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra"))

;; Customize user interface.
(menu-bar-mode 0)
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))
(setq inhibit-startup-screen t)
(column-number-mode 't)
(global-visual-line-mode 1)

;; Theme.
(load-theme 'dracula t)

;; Show stray whitespace.
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Consider a period followed by a single space to be end of sentence.
(setq sentence-end-double-space nil)

;; Use spaces, not tabs, for indentation.
(setq-default indent-tabs-mode nil)

;; Display the distance between two tab stops as 4 characters wide.
(setq-default tab-width 4)

;; Highlight matching pairs of parentheses.
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Write auto-saves and backups to separate directory.
(make-directory "~/.emacs.d/backup/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/backup/" t)))
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/margo/")))

;; Disable lockfiles.
(setq create-lockfiles nil)

;; Write customizations to ~/.emacs.d/custom.el instead of this file.
(setq custom-file "~/.emacs.d/custom.el")

;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Install packages.
(dolist (package '(markdown-mode
                   projectile
                   helm
                   company
                   compile
                   yasnippet
                   lsp-mode
                   go-projectile
                   go-mode))
  (unless (package-installed-p package)
    (package-install package)))

;; Enable Evil
(use-package evil :ensure t :init (evil-mode 1))

;; Enable Paredit.
(use-package paredit :ensure t)

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)

;; Enable Rainbow Delimiters.
(use-package rainbow-delimiters :ensure t)

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

;; Customize Rainbow Delimiters.
(set-face-foreground 'rainbow-delimiters-depth-1-face "#80ffea") ; cyan
(set-face-foreground 'rainbow-delimiters-depth-2-face "#8aff80") ; green
(set-face-foreground 'rainbow-delimiters-depth-3-face "#ffca80") ; orange
(set-face-foreground 'rainbow-delimiters-depth-4-face "#ff80bf") ; pink
(set-face-foreground 'rainbow-delimiters-depth-5-face "#9580ff") ; purple
(set-face-foreground 'rainbow-delimiters-depth-6-face "#ff9580") ; red
(set-face-foreground 'rainbow-delimiters-depth-7-face "#ffff80") ; yellow

;; Markdown Mode
(use-package markdown-mode :ensure t)

(use-package flycheck :ensure t :init (global-flycheck-mode 1))
(setq flycheck-checker-error-threshold 1000)

(use-package magit :ensure t)
(setq magit-fetch-modules-jobs 16)

(use-package projectile :ensure t :init (projectile-mode "1.0"))

(use-package helm :ensure t :init (helm-mode 1))
(require 'helm-config)
(require 'helm-projectile)
(helm-projectile-on)

(use-package company :ensure t)
(use-package compile :ensure t)
(use-package yasnippet :ensure t :init (yas-global-mode 1))

(use-package lsp-mode :ensure t)
(use-package company :ensure t)
(setq lsp-completion-provider 'capf)

;;;;;;;;;;;;;;;;;;;;;;;; Go ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'go-projectile)
(go-projectile-tools-add-path)
(setq go-projectile-tools
  '((gocode    . "github.com/mdempsky/gocode")
    (golint    . "golang.org/x/lint/golint")
    (godef     . "github.com/rogpeppe/godef")
    (errcheck  . "github.com/kisielk/errcheck")
    (godoc     . "golang.org/x/tools/cmd/godoc")
    (gogetdoc  . "github.com/zmb3/gogetdoc")
    (goimports . "golang.org/x/tools/cmd/goimports")
    (gorename  . "golang.org/x/tools/cmd/gorename")
    (gomvpkg   . "golang.org/x/tools/cmd/gomvpkg")
    (guru      . "golang.org/x/tools/cmd/guru")))

;;(require 'company-go) ; obsolete with company-lsp
(require 'go-mode)
(add-hook 'go-mode-hook #'lsp)
(add-hook 'go-mode-hook (lambda ()
  (company-mode) ; enable company upon activating go

  ;; Code layout.
  (setq tab-width 2 indent-tabs-mode 1) ; std go whitespace configuration
  (add-hook 'before-save-hook 'gofmt-before-save) ; run gofmt on each save

  ;; Shortcuts for common go-test invocations.
  (let ((map go-mode-map))
    (define-key map (kbd "C-c a") 'go-test-current-project) ;; current package, really
    (define-key map (kbd "C-c m") 'go-test-current-file)
    (define-key map (kbd "C-c .") 'go-test-current-test))

  ;; Fix parsing of error and warning lines in compiler output.
  (setq compilation-error-regexp-alist-alist ; first remove the standard conf; it's not good.
        (remove 'go-panic
                (remove 'go-test compilation-error-regexp-alist-alist)))

  ;; Make another one that works better and strips more space at the beginning.
  (add-to-list 'compilation-error-regexp-alist-alist
               '(go-test . ("^[[:space:]]*\\([_a-zA-Z./][_a-zA-Z0-9./]*\\):\\([0-9]+\\):.*$" 1 2)))

  (add-to-list 'compilation-error-regexp-alist-alist
               '(go-panic . ("^[[:space:]]*\\([_a-zA-Z./][_a-zA-Z0-9./]*\\):\\([0-9]+\\)[[:space:]].*$" 1 2)))

  ;; override.
  (add-to-list 'compilation-error-regexp-alist 'go-test t)
  (add-to-list 'compilation-error-regexp-alist 'go-panic t)))

;; CockroachDB-style Go formatter from github.com/cockroachdb/crlfmt.
;; This is also symlinked as ~/.emacs.d/gotools/bin/goimports.
(setq gofmt-command "~/src/go/bin/crlfmt")
(setq gofmt-args '("-tab" "2"))

;; Bonus: escape analysis.
(flycheck-define-checker go-build-escape
  "A Go escape checker using `go build -gcflags -m'."
  :command ("go" "build" "-gcflags" "-m"
            (option-flag "-i" flycheck-go-build-install-deps)
            ;; multiple tags are listed as "dev debug ..."
            (option-list "-tags=" flycheck-go-build-tags concat)
            "-o" null-device)
  :error-patterns
  (
   (warning line-start (file-name) ":" line ":"
          (optional column ":") " "
          (message (one-or-more not-newline) "escapes to heap")
          line-end)
   (warning line-start (file-name) ":" line ":"
          (optional column ":") " "
          (message "moved to heap:" (one-or-more not-newline))
          line-end)
   (info line-start (file-name) ":" line ":"
          (optional column ":") " "
          (message "inlining call to " (one-or-more not-newline))
          line-end)
  )
  :modes go-mode
  :predicate (lambda ()
               (and (flycheck-buffer-saved-p)
                    (not (string-suffix-p "_test.go" (buffer-file-name)))))
  :next-checkers ((warning . go-errcheck)
                  (warning . go-unconvert)
                  (warning . go-staticcheck)))

(with-eval-after-load 'flycheck
   (add-to-list 'flycheck-checkers 'go-build-escape)
   (flycheck-add-next-checker 'go-gofmt 'go-build-escape))

;; Start server.
(require 'server)
(unless (server-running-p)
  (server-start))
