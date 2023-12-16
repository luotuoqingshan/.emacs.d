;; Configure package.el to include MELPA.
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

(defun package--save-selected-packages (&rest opt) nil)

;; Ensure that use-package is installed.
;;
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

(org-babel-load-file "~/.emacs.d/configuration.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-line-numbers 'relative)
 '(display-line-numbers-type 'relative)
 '(evil-want-C-u-scroll t)
 '(global-display-line-numbers-mode t)
 '(org-agenda-files
   '("~/Dropbox/orgroam/references/YTF+Scalable2021.org" "/Users/yufanhuang/Dropbox/orgroam/augmented_lagrangian.org" "/Users/yufanhuang/Dropbox/orgroam/barvinok_pataki.org" "/Users/yufanhuang/Dropbox/orgroam/20230330113219-shoplist.org" "/Users/yufanhuang/Dropbox/orgroam/20231211141121-group_meeting.org" "/Users/yufanhuang/Dropbox/orgroam/20231209113631-research.org" "/Users/yufanhuang/Dropbox/orgroam/20231210123911-minimal_example_for_org_roam_bibtex.org" "/Users/yufanhuang/Dropbox/orgroam/20231210104845-trip.org" "/Users/yufanhuang/Dropbox/orgroam/20231209113702-interview.org" "/Users/yufanhuang/Dropbox/orgroam/20230322121329-emacs_links.org" "/Users/yufanhuang/Dropbox/orgroam/20231208193449-leetcode.org" "/Users/yufanhuang/Dropbox/orgroam/20230321122105-daily.org"))
 '(org-format-latex-options
   '(:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\["))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
