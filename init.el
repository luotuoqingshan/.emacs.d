;; init straight 
;; (load-file "~/.emacs.d/early-init.el")
(setq debug-on-error t)

;; basics
(load-file "~/.emacs.d/yh-basics.el")

;; movement
(load-file "~/.emacs.d/yh-movement.el")

;; completion, including minibuffer and in-buffer
(load-file "~/.emacs.d/yh-completion.el")

;; dictionary
(load-file "~/.emacs.d/yh-dictionary.el")

;; Org-mode
(load-file "~/.emacs.d/yh-org.el")


;; latex settings
(load-file "~/.emacs.d/yh-tex.el")

;; encryption and password storage stuff
(load-file "~/.emacs.d/yh-security.el")

;; git
(load-file "~/.emacs.d/yh-git.el")

;; yasnippet
(load-file "~/.emacs.d/yh-yasnippet.el")

;; lisp
(load-file "~/.emacs.d/yh-lisp.el")

;; julia
(load-file "~/.emacs.d/yh-julia.el")

;; markdown
(load-file "~/.emacs.d/yh-markdown.el")

;; custom
(load-file "~/.emacs.d/custom.el")


;;; copilot
(add-to-list 'load-path "~/.emacs.d/elpa/copilot.el")
(require 'editorconfig)
(require 'copilot)

(define-key copilot-completion-map (kbd "<backtab>") 'copilot-accept-completion)

;;; python

(use-package python
  :hook ((python-ts-mode . eglot-ensure))
  :mode (("\\.py\\'" . python-ts-mode))
  )
	 
