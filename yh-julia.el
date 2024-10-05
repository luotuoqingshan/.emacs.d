;; Julia
(use-package julia-mode
  :mode "\\.jl\\'")

(use-package julia-snail
  :hook (julia-mode . julia-snail-mode)
  :config
  (setq julia-snail-repl-display-eval-results t)
  )
