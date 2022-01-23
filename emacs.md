# Emacs

## Package Management

Setting up an emacs package manager is important. You can do a lot with the base emacs system but if you're used to other modern editors and especially IDEs then you know how quickly you need to start reaching for community supported plugins, packages, etc.

### MELPA

Milkypostman’s Emacs Lisp Package Archive. MELPA is the largest Emacs package repository. Each package is built on the MELPA server from upstream code using simple recipes. Packages are updated routinely throughout the day.

To use the MELPA repository, you'll need an Emacs with `package.el`, i.e., Emacs 24.1 or greater.

Enable MELPA package installation by adding an entry to your `init.el` or `.emacs` file:

```el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
```

### use-package

A good package to install after MELPA has been added is the `use-package` package. This package provides a macro, `use-package`, that allows you to isolate package cconfigurations in the `init.el` or `.emacs` file in a way that is performance-oriented and cleanh.

```el
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
```

Now you can install MELPA packages and configure them easily:

```el
(use-package foo
  :init
  (setq foo-variable t)
  :config
  (foo-mode 1))
```

It supports many more options for configuration, including commands and key binds, etc. See the [documentation here](https://github.com/jwiegley/use-package) for more.

## Packages

### Evil

Evil is the extensible vi layer for Emacs. It is a vim emulator.

```el
;; Enable Evil
(use-package evil
  :init
  :config (evil-mode 1))
```
