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


;;
;;
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

;;(defun mars/company-backend-with-yas (backends)
;;  "Add :with company-yasnippet to company BACKENDS.
;;    Taken from https://github.com/syl20bnr/spacemacs/pull/179."
;;  (if (and (listp backends) (memq 'company-yasnippet backends))
;;      backends
;;    (append (if (consp backends)
;;		backends
;;	      (list backends))
;;	    '(:with company-yasnippet))))
;;(use-package company
;;  :hook
;;  (after-init . global-company-mode)
;;  ;; add yasnippet to all backends
;;  :config
;;  (setq company-backends
;;	(mapcar #'mars/company-backend-with-yas company-backends)))


;; corfu
(use-package corfu
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  ;;(corfu-auto-delay 0.75)
  (corfu-quit-no-match nil) ; quit when the popup appears and I type anything else
  ;; Might want to customize corfu-sort-function
  :bind
  (("M-RET" . completion-at-point)
   )
  )
