(require 'use-package-ensure)
(setq use-package-always-ensure t)

(setq use-package-always-defer t)

(use-package auto-compile
	:demand t
	:config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

(setq byte-compile-warnings '(cl-functions))

(use-package use-package-ensure-system-package
	      :demand t
  :custom
  (system-packages-package-manager 'brew))

(use-package gcmh
	:demand t

	:init
	(setq gcmh-idle-delay 5
				gcmh-high-cons-threshold (* 16 1024 1024))
	:config
	(gcmh-mode))

(use-package lispy)

(use-package evil
  :demand t

  :init
  (setq evil-want-abbrev-expand-on-insert-exit nil
	evil-want-keybinding nil)

  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)

  (evil-define-key '(normal insert) 'global (kbd "C-p") 'project-find-file)

  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
  (evil-define-key 'insert org-mode-map (kbd "S-<right>") 'org-shiftright)
  (evil-define-key 'insert org-mode-map (kbd "S-<left>") 'org-shiftleft)

  (fset 'evil-visual-update-x-selection 'ignore))

(use-package evil-collection
  :after evil
  :demand t

  :config
  (setq evil-collection-mode-list
        '(deadgrep
          dired
          elfeed
          eww
          ibuffer
          info
          magit
          mu4e
          package-menu
          pdf-view
          proced
          replace
          vterm
          which-key))

  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :after (evil org)
  :demand t

  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package undo-tree
  :after (evil)
  :demand t

  :config
  (global-undo-tree-mode t))

(use-package modus-themes
  :ensure t
  :demand t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs nil 
	modus-themes-org-blocks 'tinted-background)

  (setq modus-themes-headings
    '((1 . (variable-pitch 1.5))
      (2 . (1.3))
      (agenda-date . (1.3))
      (agenda-structure . (variable-pitch light 1.8))
      (t . (1.1))))

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
	modus-themes-preset-overrides-faint)

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi-tinted :no-confirm)

  ;; use F5 to toggle modus-themes between dark and light
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;; display time in the buffer
(setq display-time-day-and-date t)
(display-time-mode 1)
;; adjust the default font size such that I don't need to zoom in/out every time
(set-face-attribute 'default nil :height 150)
;; I want my initial frame always maximized 
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
;; use cua-mode to enable C-c/C-v for copy-paste  
(setq auto-fill-mode t)
;; show column number 
(setq column-number-mode t)
;; I'm using Mac, it doesn't have <insertchar> key
(global-set-key (kbd "C-c y") 'clipboard-yank)
					;(use-package olivetti
					;  :demand t
					;  :config
					;  (olivetti-mode 1)
					;  (setq olivetti-body-width 80)
					;  :hook (text-mode LaTeX-mode))

;; use y/n always instead of yes or no 
(defalias 'yes-or-no-p 'y-or-n-p)

;;(setq-default indent-tabs-mode nil)
;;(setq-default tab-width 4)
;;(setq-default indent-line-function 'insert-tab)

;; let wq save and kill the current buffer, instead of quitting emacs
(evil-ex-define-cmd "wq" 'save-and-kill-this-buffer)
(defun save-and-kill-this-buffer()(interactive)(save-buffer)(kill-current-buffer))
;; remove the vertical scroll bars
(setq default-frame-alist '((vertical-scroll-bars . nil)))

;; always show line numbers and set the display style to relative 
;; which makes vertical move in evil more comfortable
(global-display-line-numbers-mode)
(setq display-line-numbers 'relative)

;; use moody to prettify mode line
(use-package moody
    :demand t
    :config
    (setq x-underline-at-descent-line t)
    (moody-replace-mode-line-buffer-identification)
    (moody-replace-vc-mode)
    (moody-replace-eldoc-minibuffer-message-function))

;; set up builtin dictionary
(setq dictionary-server "dict.org")
(global-set-key (kbd "C-c d s") 'dictionary-search)
(global-set-key (kbd "C-c d d") 'dictionary-lookup-definition)

(setq ispell-program-name (executable-find "hunspell"))
(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
(setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)

(global-set-key (kbd "M-\\") 'ispell-word)

(use-package counsel
  :demand t
  :init
  (ivy-mode 1))

(use-package ivy-rich
  :demand t
  :init
  (ivy-rich-mode 1))

(use-package minions
 :demand t

 :custom
 (minions-mode-line-delimiters (cons "" ""))

 :config
 (defun +set-minions-mode-line-lighter ()
   (setq minions-mode-line-lighter
         (if (display-graphic-p) "⚙" "#")))

 (add-hook 'server-after-make-frame-hook #'+set-minions-mode-line-lighter)

 (minions-mode 1))

(defun mars/company-backend-with-yas (backends)
    "Add :with company-yasnippet to company BACKENDS.
Taken from https://github.com/syl20bnr/spacemacs/pull/179."
    (if (and (listp backends) (memq 'company-yasnippet backends))
	backends
	(append (if (consp backends)
		    backends
		(list backends))
		'(:with company-yasnippet))))
(use-package company
    :hook
    (after-init . global-company-mode)
    ;; add yasnippet to all backends
    :config
    (setq company-backends
	(mapcar #'mars/company-backend-with-yas company-backends)))
;;(add-hook 'after-init-hook 'global-company-mode)

(use-package julia-mode)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((julia . t)))

;; use org-bullets to replace stars before headings
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; set initial scratch buffer to be in Org
(setq initial-major-mode 'org-mode)

(setq org-src-preserve-indentation t)

(use-package org
  :after evil
  :bind (("C-c a" . org-agenda)
	 )
  :config
  ;; turn on LaTeX-math-mode in org by default
  (setq LaTeX-math-mode t)
  ;; use Chrome to view pdfs, which enables vim key bindings via extension Vimium C
  ;; (add-to-list 'org-file-apps-macos '("\\.pdf\\", "open -a 'Google Chrome' %s"))
  :custom
  (org-directory "~/Dropbox/orgs/")
  (org-default-notes-file "~/Dropbox/orgs/inbox.org")
  (org-archive-location (concat "~/Dropbox/orgarchive/Archive-"
				(format-time-string "%Y%m" (current-time))
				".org_archive::"))
  (org-agenda-files (directory-files-recursively "~/Dropbox/orgroam" "\\.org$"))
  (org-use-fast-todo-selection 'expert)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-todo-keyword-faces
   '(("TODO" :foreground "orange" :weight bold) 
     ("NEXT" :foreground "red" :weight bold)
     ("WAITING" :foreground "blue" :weight bold)
     ("DONE" :foreground "forest green" :weight bold)
     ("CANCELLED" :foreground "cyan" :weight bold)))
  ;; Set org-latex-pdf-process to process the bibliography 
  (org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
  (org-latex-create-formula-image-program 'dvisvgm))


(defun my-org-latex-format-headline-function
    (todo todo-type priority text tags _info)
  "Default format function for a headline.
    See `org-latex-format-headline-function' for details."
  (concat
   (and todo (format "{\\framebox{\\bfseries\\rfamily\\color{%s} %s}} "
		     (pcase todo-type
		       ('todo "olive")
		       ('done "teal"))
		     todo))
   (and priority (format "\\framebox{\\#%c} " priority))
   text
   (and tags
	(format "\\hfill{}\\textsc{%s}"
		(mapconcat #'org-latex--protect-text tags ":")))))


(setq org-latex-format-headline-function 'my-org-latex-format-headline-function)

(use-package org-roam
  :after org
  ;;:demand t
  ;; setup default directory
  :custom
  (org-roam-directory "~/Dropbox/orgroam/")
  :bind (("C-c r c" . org-roam-capture)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r f" . org-roam-node-find)
         ("C-c r b" . org-roam-buffer-toggle)
         ("C-c l"   . org-latex-preview)
         )
  :config
  (setq org-roam-capture-templates '(
				     ("d" "default" plain "%?"
				      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							 "#+title: ${title}\n#+options: toc:nil\n")
				      :unnarrowed t)
				     ("r" "bibliography reference" plain "%?"
				      :target (file+head "references/${citekey}.org"
							 "#+title: ${title}\n")
				      :unnarrowed t)
				     ("m" "math notes" plain "%?" 
				      :target (file+head "${slug}.org"
							 "#+title: ${title}\n#+Latex_HEADER:\\input{/Users/yufanhuang/.emacs.d/preamble.tex}\n#+options: toc:nil"
							 )
				      :unnarrowed t)))
  
  (org-roam-db-autosync-mode t))

;; I encountered the following message when attempting
;; to export data:
;; src: https://dev.to/devteam/resolving-an-unable-to-resolve-link-error-for-org-mode-in-emacs-2n1f
;; "org-export-data: Unable to resolve link: FILE-ID"
(defun jnf/force-org-rebuild-cache ()
  "Rebuild the `org-mode' and `org-roam' cache."
  (interactive)
  (org-id-update-id-locations)
  ;; Note: you may need `org-roam-db-clear-all'
  ;; followed by `org-roam-db-sync'
  (org-roam-db-sync)
  (org-roam-update-org-id-locations))

;; https://emacs.stackexchange.com/questions/12517/how-do-i-make-the-timespan-shown-by-org-agenda-start-yesterday
;; let agenda start from yesterday
(setq org-agenda-start-day "-1d")
(setq org-agenda-span 8)
(setq org-agenda-start-on-weekday nil)

(use-package org-ref
  :demand t
  :config
  (setq
   bibtex-completion-bibliography '("~/Dropbox/bibs/yufan.bib")
   bibtex-completion-notes-path "~/Dropbox/orgroam/"
   bibtex-completion-pdf-field "file"
   bibtex-completion-pdf-open-function
   (lambda (fpath)
     (call-process shell-file-name nil 0 nil
                   shell-command-switch
                   (format "open -a 'Google Chrome' %s"
                           (shell-quote-argument fpath))))))
(define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link-hydra/body)

(use-package ivy-bibtex
  :demand t
  :after org-ref)
(use-package org-roam-bibtex
  :demand t
  :after (org-roam)
  :hook ((org-roam-mode . org-roam-bibtex-mode)
         (org-mode . org-roam-bibtex-mode))      
  ;;:bind
  ;;(("C-c r z" . orb-insert-link))
  :config
  (require 'org-ref))

;;(use-package auctex 
;;   :ensure t)
(use-package latex
  :after evil
  :ensure auctex 
  :hook ((laTeX-mode . LaTeX-math-mode)
	 (LaTeX-mode . LaTeX-math-mode)))


(use-package xenops
  :disabled
  :config
  (setq xenops-math-image-scale-factor 2.0))
					;(add-hook 'latex-mode-hook #'xenops-mode)
					;(add-hook 'LaTeX-mode-hook #'xenops-mode)  

(setq LaTeX-math-abbrev-prefix (kbd ";"))

(use-package reftex)
(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)  

(setq reftex-label-alist '(AMSTeX))
(setq doc-view-resolution 600)

(use-package yasnippet
  :demand t
  :init
  (add-hook 'latex-mode-hook #'yas-minor-mode)
  (add-hook 'LaTeX-mode-hook #'yas-minor-mode)  
  (add-hook 'org-mode-hook   #'yas-minor-mode)
  :config
  (yas-minor-mode-on)
  (setq yas/triggers-in-field t)
  (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)
  )

(use-package yasnippet-snippets
  :demand t)
;; add # condition: 'auto for auto expand
(defun my-yas-try-expanding-auto-snippets ()
  (when yas-minor-mode
    (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
      (yas-expand))))
(add-hook 'post-command-hook #'my-yas-try-expanding-auto-snippets)

(use-package magit
  :demand t)
