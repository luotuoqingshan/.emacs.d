;; init straight 
;; (load-file "~/.emacs.d/early-init.el")
;; (setq debug-on-error t)

(if (display-graphic-p)
  ;; init only for gui
  (progn
    ;; markdown
    (load-file "~/.emacs.d/yh-markdown.el")
      
    ;; dictionary
    (load-file "~/.emacs.d/yh-dictionary.el")

    ;; Org-mode
    (load-file "~/.emacs.d/yh-org.el")


    ;; latex settings
    (load-file "~/.emacs.d/yh-tex.el")

    ;; encryption and password storage stuff
    (load-file "~/.emacs.d/yh-security.el")
    )
  ;; init only for terminal
  ()
  )

;; basics
(load-file "~/.emacs.d/yh-basics.el")

;; movement
(load-file "~/.emacs.d/yh-movement.el")

;; completion, including minibuffer and in-buffer
(load-file "~/.emacs.d/yh-completion.el")



;; git
(load-file "~/.emacs.d/yh-git.el")

;; yasnippet
(load-file "~/.emacs.d/yh-yasnippet.el")

;; lisp
(load-file "~/.emacs.d/yh-lisp.el")

;; julia
(load-file "~/.emacs.d/yh-julia.el")



;; custom
(load-file "~/.emacs.d/custom.el")


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

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))
