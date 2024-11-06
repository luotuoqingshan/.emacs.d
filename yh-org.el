
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
