;; init straight 
;; (load-file "~/.emacs.d/early-init.el")
;; (setq debug-on-error t)

;;; basics

;; Configure package.el to include MELPA.
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

(straight-use-package 'use-package)

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

;;  always :ensure t
 (require 'use-package-ensure)
 (setq use-package-always-ensure t)

;; (setq straight-use-package-by-default t)

;; always defer loading packages unless :demand t
(setq use-package-always-defer t)

;; Always compile packages, and use the newest version available.
(use-package auto-compile
  :straight nil
  :demand t
  :config (auto-compile-on-load-mode))


(setq load-prefer-newer t)

;; add PATHs from shell
;; (use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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
  (load-theme 'modus-operandi-deuteranopia :no-confirm)

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

;; do not display menu bar
(menu-bar-mode -1)

;; disalbe mode-line
;; (setq-default header-line-format mode-line-format)
;; (setq-default mode-line-format nil)

;; always show line numbers and set the display style to relative 
(global-display-line-numbers-mode)
(global-visual-line-mode 1)
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



;;; outline
(use-package outline
  :ensure nil ; built-in

  :hook ((emacs-lisp-mode . outline-minor-mode)
         (LaTeX-mode . outline-minor-mode)
         (bibtex-mode . outline-minor-mode))

  :bind (:map outline-minor-mode-map
         ([C-tab] . bicycle-cycle)
         ("<backtab>" . bicycle-cycle-global)

         ;; create separate keymap, otherwise Org gets confused
         (:prefix-map my/outline-minor-mode-prefix-map)
         (:prefix "M-o")
         :map my/outline-minor-mode-prefix-map
         ;; movement
         ("n" . outline-next-visible-heading)
         ("p" . outline-previous-visible-heading)
         ("f" . outline-forward-same-level)
         ("b" . outline-backward-same-level)
         ;; folding
         ("t" . outline-hide-body)
         ("a" . outline-show-all)
         ("h" . outline-hide-entry)
         ("s" . outline-show-entry)))

(use-package bicycle
  ;; Make the bindings in the 'outline' use-package block.
  )




;; init only for graphic system
(if (display-graphic-p)
  ;; init only for gui
  (progn

;;; markdown
    (setq markdown-command "~/anaconda3/bin/pandoc")

;;; dictionary

    ;; Dictionary
    ;; I want to use builtin dictionary to search some unknown words.
    ;; set up builtin dictionary
    (setq dictionary-server "dict.org")
    (global-set-key (kbd "C-c d s") 'dictionary-search)
    (global-set-key (kbd "C-c d d") 'dictionary-lookup-definition)

;;; Org-mode

    ;; use org-superstar to replace stars before headings
    (require 'org-superstar)
    (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
    
    ;; set initial scratch buffer to be in Org
    (setq initial-major-mode 'org-mode)
    
    (setq org-src-preserve-indentation t)
    
    (use-package org
      :bind (("C-c a" . org-agenda)
    	  )
      :hook
      ((org-mode . org-indent-mode))
      :config
      ;; turn on LaTeX-math-mode in org by default
      ;; (setq LaTeX-math-mode t)
      ;; use Chrome to view pdfs, which enables vim key bindings via extension Vimium C
      ;; (add-to-list 'org-file-apps-macos '("\\.pdf\\", "open -a 'Google Chrome' %s"))
      :custom
      (org-directory "~/Dropbox/orgroam/")
      (org-default-notes-file "~/Dropbox/orgs/inbox.org")
      (org-archive-location (concat "~/Dropbox/orgarchive/Archive-"
    				(format-time-string "%Y%m" (current-time))
    				".org_archive::"))
      (org-agenda-files (list "inbox.org" "agenda.org" "projects.org" "paper.org"))
      (org-use-fast-todo-selection 'expert)
      (org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)")))
      (org-todo-keyword-faces
        '(("TODO" :foreground "orange" :weight bold) 
           ("NEXT" :foreground "red" :weight bold)
           ("WAIT" :foreground "blue" :weight bold)
           ("DONE" :foreground "forest green" :weight bold)))
      (org-agenda-prefix-format
        '((agenda . " %i %?-12t% s")
          (todo . "  ")
          (tags . " %i %-12:c")
          (search . " %i %-12:c"))
      )
      ;; Set org-latex-pdf-process to process the bibliography 
      (org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
      ;; add the following line to enable diary
      ;; e.g. adding anniversaries 
      (org-agenda-include-diary t)
      (org-latex-create-formula-image-program 'dvisvgm)
      (org-file-apps
        '((auto-mode . emacs)
           (directory . emacs)
           ("\\.mm\\'" . default)
           ("\\.x?html?\\'" . default)
           ("\\.pdf\\'" . "open -a \"Google Chrome\" %s")))
      (org-format-latex-options
        '(:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
           ("begin" "$1" "$" "$$" "\\(" "\\[")))
      )
    
    
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
    
    (defun org-roam-capture-inbox ()
         (interactive)
         (call-interactively 'org-store-link)
         (org-roam-capture nil "i"))
    
    (use-package org-roam
      :after org
      ;;:demand t
      ;; setup default directory
      :custom
      (org-roam-directory "~/Dropbox/orgroam/")
      (org-roam-dailies-capture-templates
       '(("d" "default" entry "* %<%I:%M %p>: %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
      (org-agenda-hide-tags-regexp ".")
      :bind (("C-c r c" . org-roam-capture)
             ("C-c r i" . org-roam-node-insert)
             ("C-c r f" . org-roam-node-find)
             ("C-c r b" . org-roam-buffer-toggle)
             ("C-c l"   . org-latex-preview)
    	 ("C-c i"   . org-roam-capture-inbox) 
             :map org-roam-dailies-map
    	 ("Y" . org-roam-dailies-capture-yesterday)
             ("T" . org-roam-dailies-capture-tomorrow)
             )
      :bind-keymap
      ("C-c r d" . org-roam-dailies-map)
      :config
      (require 'org-roam-dailies)
      (setq org-roam-capture-templates
        '(
           ("d" "default" plain "%?"
    	 :target (file+head "${slug}.org"
    		   "#+title: ${title}\n#+options: toc:nil\n")
    	 :unnarrowed t)
           ("i" "inbox" entry "* TODO %?\n/Entered on/ %U"
    	 :target (file+head "inbox.org"
    		   ("Inbox items"))
    	 :unnarrowed t)
           ("m" "meeting" plain "* %?"
    	 :target (file+olp "agenda.org"
    		   "Future"))
           ("r" "bibliography reference" plain "%?"
    	 :target (file+head "references/${citekey}.org"
    		   "#+title: ${title}\n")
    	 :unnarrowed t)
           ("l" "math notes" plain "%?" 
    	 :target (file+head "${slug}.org"
    		   "#+title: ${title}\n#+Latex_HEADER:\\input{~/Documents/latex-templates/headers.tex}\n#+options: toc:nil"
    		   )
    	 :unnarrowed t)))
      
      (org-roam-db-autosync-mode t)
      )
    
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
    (setq org-agenda-span 10)
    (setq org-agenda-start-on-weekday nil)
    
    ;; Org-ref
    
    (use-package org-ref
      :demand t
      :config
      (setq
       bibtex-completion-bibliography '("~/Dropbox/bibs/yufan_ebib.bib" "~/Dropbox/bibs/yufan.bib")
       bibtex-completion-notes-path "~/Dropbox/orgroam/"
       bibtex-completion-pdf-field "file"
       bibtex-completion-pdf-open-function
       (lambda (fpath)
         (call-process shell-file-name nil 0 nil
                       shell-command-switch
                       (format "open -a 'Google Chrome' %s"
                               (shell-quote-argument fpath))))))
    (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link-hydra/body)
    
    ;; https://www.labri.fr/perso/nrougier/GTD/index.html
    (setq org-agenda-custom-commands
          '(("g" "Get Things Done (GTD)"
             ((agenda ""
                      ((org-agenda-skip-function
                        '(org-agenda-skip-entry-if 'deadline))
                       (org-deadline-warning-days 0)))
              (todo "NEXT"
                    ((org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'deadline))
                     (org-agenda-prefix-format "  %i %-12:c [%e] ")
                     (org-agenda-overriding-header "\nTasks\n")))
              (agenda nil
                      ((org-agenda-entry-types '(:deadlinne))
                       (org-agenda-format-date "")
                       (org-deadline-warning-days 7)
                       (org-agenda-skip-function
                        '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                       (org-agenda-overriding-header "\nDeadlines")))
              (tags-todo "inbox"
                         ((org-agenda-prefix-format "  %?-12t% s")
                           (org-agenda-overriding-header "\nInbox\n")))
    	   (tags-todo "paper"
    	     ((org-agenda-prefix-format "  %?-12t% s")
                   (org-agenda-overriding-header "\nPaper List\n")))
    	     
              (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted today\n")))))))
    
    (setq org-log-done 'time)
    
    
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



;;; latex settings

    ;; CDLaTeX
    (use-package cdlatex
    
      ;; cdlatex is similar to LaTeX-math-mode and yasnippet but I feel it
      ;; is more powerful, take a look at the github repo for more info
      ;; https://github.com/cdominik/cdlatex
    
      :init
      ;; (add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)
      ;; it's important to set this prefix before loading otherwise it
      ;; won't take effect
      (setq cdlatex-math-symbol-prefix ?\;)
    
      :hook ((LaTeX-mode . turn-on-cdlatex))
    
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
          (?* ("\\times" "\\otimes"))
          (?+ ("\\cup" "\\oplus"))
          (?- ("\\cap" "\\leftrightarrow" "\\longleftrightarrow")) 
          (?. ("\\cdot" "\\ldots")) 
          (?< ("\\leftarrow" "\\Leftarrow" "\\min"))))
      
      (setq cdlatex-math-modify-alist
        '((?b "\\mathbb" "\\textbf" t nil nil)
          (?s nil "\\textsc" t nil nil)))
      (setq cdlatex-command-alist
        '(("ge"  "Insert \\geq" "\\geq" nil nil nil t)
          ("lr||" "Insert \\left\\| \\right\\|" "\\left\\| ? \\right\\|" cdlatex-position-cursor nil nil t)
          ("le"  "Insert \\leq" "\\leq" nil nil nil t)
          ("ne"  "Insert \\neq" "\\neq" nil nil nil t)
          ("mi"  "Insert \\min" "\\min" nil nil nil t)
          ("ma"  "Insert \\max" "\\max" nil nil nil t)
          ("psd" "Insert \\succeq" "\\succeq" nil nil nil t)
          ("pd"  "Insert \\succ" "\\succ" nil nil nil t)
          ("nsd" "Insert \\preceq" "\\preceq" nil nil nil t)
          ("nd"  "Insert \\prec" "\\prec" nil nil nil t)
          ("nn"  "Insert \\nonumber" "\\nonumber" nil nil nil t)
          ("sum" "Insert \\sum_{}^{}" "\\sum_{?}^{}" cdlatex-position-cursor nil nil t)
          ("prod" "Insert \\prod_{}^{}" "\\prod_{?}^{}" cdlatex-position-cursor nil nil t)
          ("prodl" "Insert \\prod\\limits_{}^{}" "\\prod\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
          ("norm" "Insert \\| \\|" "\\|?\\|" cdlatex-position-cursor nil nil t)
          ("floor" "Insert \\lfloor \\rfloor" "\\lfloor ? \\rfloor" cdlatex-position-cursor nil nil t)
          ("ceil" "Insert \\lceil \\rceil" "\\lceil ? \\rceil" cdlatex-position-cursor nil nil t)
           )))
    
    
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
      
    ;; add Cleveref style into reftex 
      (add-hook 'LaTeX-mode-hook
        (lambda () (setq reftex-ref-style-default-list '("Default" "Cleveref"))))
      ;;(add-hook 'LaTeX-mode-hook (lambda () (company-mode nil)))
       :hook (;;(LaTeX-mode . LaTeX-math-mode)
            (LaTeX-mode . prettify-symbols-mode))
    	  ;; turn off company-mode manually, use corfu instead
    	  ;; I don't understand why company-mode is on by default 
    
    
       :config
       ;; use pdflatex
       ;; (setq latex-run-command "pdflatex")
       (setq TeX-view-program-list '(("Google Chrome" "open -a 'Google Chrome' %o" "open")))
       (setq TeX-view-program-selection '((output-pdf "Google Chrome")))
       (setq TeX-auto-save t)
       (setq TeX-parse-self t)
       (setq-default TeX-master nil)
       ;;(setq prettify-symbols-unprettify-at-point nil)
      )
    ;; (add-to-list 'post-command-hook #'TeX-view)
    
    
    (use-package reftex
      :hook (LaTeX-mode . turn-on-reftex)
      :custom
      (reftex-label-alist '(AMSTeX))
      (doc-view-resolution 600))
    ;;(add-hook 'LaTeX-mode-hook 'turn-on-reftex)  
    
    ;;(setq reftex-label-alist '(AMSTeX))
    ;;(setq doc-view-resolution 600)
    
    (add-to-list 'load-path "~/.emacs.d/elpa/lsp-latex-20240803.1436/")
    (require 'lsp-latex)
    ;; "texlab" executable must be located at a directory contained in `exec-path'.
    ;; If you want to put "texlab" somewhere else,
    ;; you can specify the path to "texlab" as follows:
    ;; (setq lsp-latex-texlab-executable "/path/to/texlab")
    
    (with-eval-after-load "tex-mode"
      (add-hook 'LaTeX-mode-hook 'lsp))
    
    ;;  For bibtex
    (with-eval-after-load "bibtex"
      (add-hook 'bibtex-mode-hook 'lsp))
    
    ;; ebib is a bib management tool 
    (use-package ebib
      :after biblio
      :bind (:map ebib-index-mode-map
                  ("B" . ebib-biblio-import-doi)
                  :map biblio-selection-mode-map
                  ("e" . ebib-biblio-selection-import)))
    
    ;; biblio is a tool for fetching bib files from online sources 
    (use-package biblio)


  


;;; encryption and password storage stuff
    (require 'epa-file)
    (epa-file-enable)
    (setq epa-file-encrypt-to '("huan1754@purdue.edu"))

;;; end of 1st statement of if 
    )
  ;; init only for terminal
  ()
  )


;;; movement

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

;; turn on pixel-scroll-precision-mode by default
(pixel-scroll-precision-mode 1)



;; avy is a tool for faster jumping around in the file 
(use-package avy
  :demand t
  :config
  (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  (global-set-key (kbd "M-g l") 'avy-goto-line)
  (global-set-key (kbd "M-g c") 'avy-goto-char-timer))


;; isearch
;; use C-Return to let cursor stay at the front of the match
(define-key isearch-mode-map [(control return)]
  #'isearch-exit-other-end)
(defun isearch-exit-other-end ()
  "Exit isearch, at the opposite end of the string."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))


;;; completion, including minibuffer and in-buffer

;; Config =ivy=
;; I use =ivy= and =counsel= as my completion framework.



;;(use-package counsel
;;  :demand t
;;  :init
;;  (ivy-mode 1))
;;
;;(use-package ivy-rich
;;  :demand t
;;  :init
;;  (ivy-rich-mode 1))

 


(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; completion UI style
;; Enable vertico
(use-package vertico
  ;;:custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; completion annotations
;; copied from marginalia README
;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))


;; Company-mode
;; I use company-mode for completion.

;; (defun mars/company-backend-with-yas (backends)
;;   "Add :with company-yasnippet to company BACKENDS.
;;     Taken from https://github.com/syl20bnr/spacemacs/pull/179."
;;   (if (and (listp backends) (memq 'company-yasnippet backends))
;;       backends
;;     (append (if (consp backends)
;; 		backends
;; 	      (list backends))
;; 	    '(:with company-yasnippet))))
;; (use-package company
;;   :hook
;;   (after-init . global-company-mode)
;;   ;; add yasnippet to all backends
;;   :config
;;   (setq company-backends
;; 	(mapcar #'mars/company-backend-with-yas company-backends)))


;; corfu
(use-package corfu
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  ;;(corfu-auto-delay 0.75)
  (corfu-quit-no-match t) ; quit when the popup appears and I type anything else
  ;; Might want to customize corfu-sort-function
  :bind
  (("M-RET" . completion-at-point)
   )
  )

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)





;;; git
;; Magit
(use-package magit
  :bind ("C-x g" . magit-status))


;;; yasnippet


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


;;; lisp
(use-package lispy
  :bind ("<f6>" . lispy-mode))
;; use 2 spaces indentation
(setq lisp-indent-offset 2)

(use-package elisp-mode
  :ensure nil ; built-in
  :hook ((emacs-lisp-mode . my/elisp-outline-regexp-setup))

  :config
  (defun my/elisp-outline-regexp-setup ()
    ;; The default outline-regexp for elisp-mode includes autoloads and any
    ;; lisp structure starting at the beginning of the line, which is
    ;; confusing. The default value is defined in 'lisp-mode.el':
    ;; (setq-local outline-regexp ";;;\\(;* [^ \t\n]\\|###autoload\\)\\|(")
    ;; Better results with:
    (setq-local outline-regexp ";;;\\(;* [^ \t\n]\\)")))


;;; Julia
(use-package julia-mode
  :mode "\\.jl\\'")

(use-package julia-snail
  :hook (julia-mode . julia-snail-mode)
  :config
  (setq julia-snail-repl-display-eval-results t)
  )



;;; custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
    '("78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" default)))
 

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  )


;;; copilot
(add-to-list 'load-path "~/.emacs.d/elpa/copilot.el")
(require 'editorconfig)
(require 'copilot)
;; (global-copilot-mode 1)

(define-key copilot-completion-map (kbd "<backtab>") 'copilot-accept-completion)

;;; python

(use-package python
  :hook ((python-ts-mode . eglot-ensure))
  :mode (("\\.py\\'" . python-ts-mode))
  )

;;; cpp
(defun compile-cpp ()
  "Compile the current C++ file."
  (interactive)
  (save-buffer)  ;; Save the buffer before compiling
  (compile (format "g++ -std=c++17  -Wall -o %s %s"
                   (file-name-sans-extension (buffer-file-name))  ;; output file
                   (buffer-file-name))))  ;; current file

(use-package cpp
  :hook
  ((c++-ts-mode . eglot-ensure)
    (c++-ts-mode . (lambda()
		     (local-set-key (kbd "<f5>") 'compile-cpp))))

  :mode (("\\.cpp\\'" . c++-ts-mode))
  )

;;; lsp-mode
(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))


