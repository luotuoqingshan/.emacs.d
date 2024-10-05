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
