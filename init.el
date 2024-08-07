;; Configure package.el to include MELPA.
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

(defun package--save-selected-packages (&rest opt) nil)

;; Ensure that use-package is installed.
;; See doc of use-package at https://github.com/jwiegley/use-package
;; If use-package isn't already installed, it's extremely likely that this is a
;; fresh installation! So we'll want to update the package repository and
;; install use-package before loading the literate configuration.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; To help debug, we enable more verbose when emacs is called with --debug-init
(if init-file-debug
    (setq use-package-verbose t
            use-package-expand-minimally nil
            use-package-compute-statistics t
            debug-on-error t)
    (setq use-package-verbose nil
          use-package-expand-minimally t))

;;(org-babel-load-file "~/.emacs.d/configuration.org")

;; always :ensure t
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; always defer loading packages unless :demand t
(setq use-package-always-defer t)

;; Always compile packages, and use the newest version available.
(use-package auto-compile
  :demand t
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

;; Disable deprecation warnings about =cl=. The =cl= library has been deprecated, but
;; lots of packages still use it.
(setq byte-compile-warnings '(cl-functions))

;; If an Emacs package relies on the installation of a system package, install that
;; package (for example, =deadgrep= doesn't work if =ripgrep= isn't installed). This
;; uses the system package manager (Debian's =apt=, in my case).
(use-package use-package-ensure-system-package
  :demand t
  :custom
  (system-packages-package-manager 'brew))

;; Trigger garbage collection when I've been idle for five seconds and memory usage
;; is over 16 MB.
(use-package gcmh
  :demand t
  :init
  (setq gcmh-idle-delay 5
	gcmh-high-cons-threshold (* 16 1024 1024))
  :config
  (gcmh-mode))

;; Basic Keyboard setup
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)



;; Themes and faces
(use-package modus-themes
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

;; Basic Setup
;; Some basic and general setups.

;; display time in the buffer
(setq display-time-day-and-date t)
(display-time-mode 1)

;; adjust the default font size such that I don't need to zoom in/out every time
(set-face-attribute 'default nil :height 150)

;; I want my initial frame always maximized 
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; show column number 
(setq column-number-mode t)

;; use y/n always instead of yes or no 
(defalias 'yes-or-no-p 'y-or-n-p)

;; remove the vertical scroll bars
(setq default-frame-alist '((vertical-scroll-bars . nil)))

;; always show line numbers and set the display style to relative 
(global-display-line-numbers-mode)
(setq display-line-numbers 'relative)

(use-package vterm)

;; use moody to prettify mode line
(use-package moody
  :demand t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))


;; pixel-scroll for smoother scrolling
;; see discussion at https://www.reddit.com/r/emacs/comments/wx7ytn/emacs_29_native_smooth_scrolling/
;; and config at https://github.com/VictorYYW/.emacs.d/blob/main/init.el#L135
(require 'pixel-scroll)
(defun pixel-scroll-half-page-down ()
  "Smoothly scroll down half the current window height."
  (interactive)
  (pixel-scroll-precision-interpolate (- (/ (window-text-height nil t) 2))
                                      nil 1))
(defun pixel-scroll-half-page-up ()
  "Smoothly scroll down half the current window height."
  (interactive)
  (pixel-scroll-precision-interpolate (/ (window-text-height nil t) 2)
                                        nil 1))

(define-key pixel-scroll-precision-mode-map (kbd "M-p") #'pixel-scroll-half-page-up)
(define-key pixel-scroll-precision-mode-map (kbd "M-n") #'pixel-scroll-half-page-down)


;; isearch
;; use C-Return to let cursor stay at the front of the match
(define-key isearch-mode-map [(control return)]
  #'isearch-exit-other-end)
(defun isearch-exit-other-end ()
  "Exit isearch, at the opposite end of the string."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))


;; Dictionary
;; I want to use builtin dictionary to search some unknown words.
;; set up builtin dictionary
(setq dictionary-server "dict.org")
(global-set-key (kbd "C-c d s") 'dictionary-search)
(global-set-key (kbd "C-c d d") 'dictionary-lookup-definition)

;; Config =ivy=
;; I use =ivy= and =counsel= as my completion framework.

(use-package counsel
  :demand t
  :init
  (ivy-mode 1))

(use-package ivy-rich
  :demand t
  :init
  (ivy-rich-mode 1))

;; Use =minions= to hide all minor modes
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

;; Company-mode
;; I use company-mode for completion.

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


;; Org-mode
;; use org-bullets to replace stars before headings
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; set initial scratch buffer to be in Org
(setq initial-major-mode 'org-mode)

(setq org-src-preserve-indentation t)

(use-package org
  :bind (("C-c a" . org-agenda)
	 )
  :config
  ;; turn on LaTeX-math-mode in org by default
  ;; (setq LaTeX-math-mode t)
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


;; Org-Roam

(use-package org-roam
  :after org
  ;;:demand t
  ;; setup default directory
  :custom
  (org-roam-directory "~/Dropbox/orgroam/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c r c" . org-roam-capture)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r f" . org-roam-node-find)
         ("C-c r b" . org-roam-buffer-toggle)
         ("C-c l"   . org-latex-preview)
         :map org-roam-dailies-map
	 ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow)
         )
  :bind-keymap
  ("C-c r d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
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
							 "#+title: ${title}\n#+Latex_HEADER:\\input{/Users/yufanhuang/Documents/latex-templates/headers.tex}\n#+options: toc:nil"
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

;; Org-Agenda

;; https://emacs.stackexchange.com/questions/12517/how-do-i-make-the-timespan-shown-by-org-agenda-start-yesterday
;; let agenda start from yesterday
(setq org-agenda-start-day "-1d")
(setq org-agenda-span 8)
(setq org-agenda-start-on-weekday nil)

;; Org-ref

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


;; Org-Roam-Bibtex 

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

;; CDLaTeX
(use-package cdlatex

  ;; cdlatex is similar to LaTeX-math-mode but I feel it is more
  ;; powerful, take a look at the github repo for more info
  ;; https://github.com/cdominik/cdlatex

  :init
  (add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)
  ;; it's important to set this prefix before loading otherwise it
  ;; won't take effect
  (setq cdlatex-math-symbol-prefix ?\;)

  :config
  ;; I don't want _ and ^ expanded to sub and super scripts
  ;; automatically outside math mode
  (setq cdlatex-sub-super-scripts-outside-math-mode nil)
  (setq cdlatex-math-symbol-alist
    '((?e ("\\varepsilon" "\\epsilon" "\\exp"))
      (?f ("\\varphi" "\\phi"))
      (?\[ ("\\subseteq"))
      (?\] ("\\supseteq"))
      (?M ("\\max" "\\min"))
      (?> ("\\rightarrow" "\\Rightarrow" "\\max"))
      (?: ("\\coloneqq"))
      (?- ("\\cap" "\\leftrightarrow" "\\longleftrightarrow"))
      (?< ("\\leftarrow" "\\Leftarrow" "\\min"))))
  (setq cdlatex-math-modify-alist
    '((?b "\\mathbb" "\\textbf" t nil nil)
      (?s nil "\\textsc" t nil nil)))
  (setq cdlatex-command-alist
    '(("ge"  "Insert \\geq" "\\geq" nil nil nil t)
      ("le"  "Insert \\leq" "\\leq" nil nil nil t)
      ("ne"  "Insert \\neq" "\\neq" nil nil nil t)
      ("mi"  "Insert \\min" "\\min" nil nil nil t)
      ("ma"  "Insert \\max" "\\max" nil nil nil t)
      ("psd" "Insert \\succeq" "\\succeq" nil nil nil t)
      ("pd"  "Insert \\succ" "\\succ" nil nil nil t)
      ("nsd" "Insert \\precceq" "\\precceq" nil nil nil t)
      ("nd"  "Insert \\precc" "\\precc" nil nil nil t)
      ("norm" "Insert \\| \\|" "\\|?\\|" cdlatex-position-cursor nil nil t)
      ("floor" "Insert \\lfloor \\rfloor" "\\lfloor ? \\rfloor" cdlatex-position-cursor nil nil t)
      ("ceil" "Insert \\lceil \\rceil" "\\lceil ? \\rceil" cdlatex-position-cursor nil nil t) 
      ("article" "Insert an article template"
       "\\documentclass[11pt]{article}

\\usepackage[letterpaper, margin=1in]{geometry}
\\usepackage[utf8]{inputenc} % allow utf-8 input
\\usepackage[T1]{fontenc}    % use 8-bit T1 fonts
\\usepackage[colorlinks,linkcolor=black,citecolor=black]{hyperref}       % hyperlinks
\\usepackage{url}            % simple URL typesetting
\\usepackage{booktabs}       % professional-quality tables
\\usepackage{amsfonts}       % blackboard math symbols
\\usepackage{nicefrac}       % compact symbols for 1/2, etc.
\\usepackage{microtype}      % microtypography
\\usepackage{xcolor}
\\input{/Users/yufanhuang/.emacs.d/preamble}

\\title{?}
\\author{Yufan Huang}
\\date{\today}
\\begin{document}
\\maketitle

\\end{document}" cdlatex-position-cursor nil t nil))))


;; Auctex/Latex

(use-package latex

  ;; https://ftp.gnu.org/pub/gnu/auctex/11.88-extra/tex-ref.pdf
  ;; a reference card for auctex  

  :ensure auctex 

  :init 
  (add-hook 'LaTeX-mode-hook
    (defun preview-larger-previews ()
      (setq preview-scale-function
        (lambda () (* 1.25
  		     (funcall (preview-scale-from-face)))))))
  (add-hook 'LaTeX-mode-hook
    (lambda () (setq reftex-ref-style-default-list '("Default" "Cleveref"))))
  :hook (;;(laTeX-mode . LaTeX-math-mode)
	 ;;(LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . prettify-symbols-mode))
  :config
  (setq TeX-view-program-list '(("Google Chrome" "open -a 'Google Chrome' %o" "open")))
  (setq TeX-view-program-selection '((output-pdf "Google Chrome")))
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  ;;(setq prettify-symbols-unprettify-at-point nil)
  )

(use-package reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)  

(setq reftex-label-alist '(AMSTeX))
(setq doc-view-resolution 600)

;;Yasnippet
;;#+begin_src emacs-lisp
;;(use-package yasnippet
;;  :demand t
;;  :init
;;  (add-hook 'latex-mode-hook #'yas-minor-mode)
;;  (add-hook 'LaTeX-mode-hook #'yas-minor-mode)  
;;  (add-hook 'org-mode-hook   #'yas-minor-mode)
;;  :config
;;  (yas-minor-mode-on)
;;  (setq yas/triggers-in-field t)
;;  (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)
;;  )

;;(use-package yasnippet-snippets
;;  :demand t
;;  :init
;;  (yas--remove-template-by-uuid (yas--table-get-create 'latex-mode) "frame"))
;;(yas--remove-template-by-uuid (yas--table-get-create 'latex-mode) "frame")
;; add # condition: 'auto for auto expand
;;(defun my-yas-try-expanding-auto-snippets ()
;;  (when yas-minor-mode
;;    (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
;;      (yas-expand))))
;;(add-hook 'post-command-hook #'my-yas-try-expanding-auto-snippets)


;; Magit
(use-package magit
  :demand t)


;; avy is a tool for faster jumping around in the file 
(use-package avy
  :demand t
  :config
  (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  (global-set-key (kbd "M-g l") 'avy-goto-line)
  (global-set-key (kbd "M-g c") 'avy-goto-char-timer))


;; lisp
(use-package lispy
  :bind ("<f6>" . lispy-mode))
;; use 2 spaces indentation
(setq lisp-indent-offset 2)

;; Julia
(use-package julia-mode
  :mode "\\.jl\\'")

(use-package julia-snail
  :hook (julia-mode . julia-snail-mode)
  :config
  (setq julia-snail-repl-display-eval-results t)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; use pandoc to view markdown
 '(markdown-command "/Users/yufanhuang/anaconda3/bin/pandoc")
 '(custom-safe-themes
   '("78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" default))
 '(display-line-numbers 'relative)
 '(display-line-numbers-type 'relative)
 '(evil-want-C-u-scroll t)
 '(global-display-line-numbers-mode t)
 '(org-agenda-files
   '("~/Dropbox/orgroam/") nil nil "Customized with use-package org")
 '(org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "open -a \"Google Chrome\" %s")))
 '(org-format-latex-options
   '(:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\["))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
