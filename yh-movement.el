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
