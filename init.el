(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(use-package evil
  :ensure t
  :config
  (evil-mode)
  (evil-set-undo-system 'undo-tree))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t))

(use-package auctex
  :ensure t
  :defer t)

(add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
(setq LaTeX-math-abbrev-prefix (kbd ";"))

(require 'cl-lib)
(require 'doc-view)
(setq doc-view-resolution 600)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(setq custom-file "~/.emacs.d/custom.el")

