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

;; add PATHs from shell
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
