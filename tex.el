;; CDLaTeX
(use-package cdlatex

  ;; cdlatex is similar to LaTeX-math-mode and yasnippet but I feel it
  ;; is more powerful, take a look at the github repo for more info
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
  
  ;; add Cleveref style into reftex 
  (add-hook 'LaTeX-mode-hook
    (lambda () (setq reftex-ref-style-default-list '("Default" "Cleveref"))))
  :hook (;;(laTeX-mode . LaTeX-math-mode)
	 ;;(LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . prettify-symbols-mode))
  :config
  ;; use pdflatex
  (setq latex-run-command "pdflatex")
  (setq TeX-view-program-list '(("Google Chrome" "open -a 'Google Chrome' %o" "open")))
  (setq TeX-view-program-selection '((output-pdf "Google Chrome")))
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  ;;(setq prettify-symbols-unprettify-at-point nil)
  )
;; (add-to-list 'post-command-hook #'TeX-view)


(use-package reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)  

(setq reftex-label-alist '(AMSTeX))
(setq doc-view-resolution 600)
