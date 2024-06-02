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
   '("/Users/yufanhuang/Dropbox/orgroam/20240527130601-makefile.org" "/Users/yufanhuang/Dropbox/orgroam/20231209113631-research.org" "/Users/yufanhuang/Dropbox/orgroam/20240219120737-a_phd_is_not_enough.org" "/Users/yufanhuang/Dropbox/orgroam/20240106135803-parallel_computing.org" "/Users/yufanhuang/Dropbox/orgroam/math/test.org" "/Users/yufanhuang/Dropbox/orgroam/references/ACFinding2009.org" "/Users/yufanhuang/Dropbox/orgroam/references/AMSOptimization2008a.org" "/Users/yufanhuang/Dropbox/orgroam/references/ANApproximating2004.org" "/Users/yufanhuang/Dropbox/orgroam/references/BCComputational2006.org" "/Users/yufanhuang/Dropbox/orgroam/references/BHBEffectively2008.org" "/Users/yufanhuang/Dropbox/orgroam/references/BMLocal2005.org" "/Users/yufanhuang/Dropbox/orgroam/references/BMNonlinear2003.org" "/Users/yufanhuang/Dropbox/orgroam/references/BVBDeterministic2020.org" "/Users/yufanhuang/Dropbox/orgroam/references/BVBNonconvex2016.org" "/Users/yufanhuang/Dropbox/orgroam/references/CLCNonconvex2019.org" "/Users/yufanhuang/Dropbox/orgroam/references/CMPolynomial2022.org" "/Users/yufanhuang/Dropbox/orgroam/references/CifBurer2021.org" "/Users/yufanhuang/Dropbox/orgroam/references/DWComputational2022.org" "/Users/yufanhuang/Dropbox/orgroam/references/FPKDense2001.org" "/Users/yufanhuang/Dropbox/orgroam/references/GRNHEMI2016.org" "/Users/yufanhuang/Dropbox/orgroam/references/KKTMaximizing2003.org" "/Users/yufanhuang/Dropbox/orgroam/references/LWLBInfluence2023.org" "/Users/yufanhuang/Dropbox/orgroam/references/OSVBurerMonteiro2022.org" "/Users/yufanhuang/Dropbox/orgroam/references/PVBBMaximum2013.org" "/Users/yufanhuang/Dropbox/orgroam/references/SZCausal2023.org" "/Users/yufanhuang/Dropbox/orgroam/references/VarMatrix2000.org" "/Users/yufanhuang/Dropbox/orgroam/references/WWRank2020.org" "/Users/yufanhuang/Dropbox/orgroam/references/YTF+Scalable2021.org" "/Users/yufanhuang/Dropbox/orgroam/references/ZZG+Social2019.org" "/Users/yufanhuang/Dropbox/orgroam/templates/mathnotes.org" "/Users/yufanhuang/Dropbox/orgroam/20230312181955-climbing.org" "/Users/yufanhuang/Dropbox/orgroam/20230313164752-org_links.org" "/Users/yufanhuang/Dropbox/orgroam/20230313165639-tex_links.org" "/Users/yufanhuang/Dropbox/orgroam/20230321122105-daily.org" "/Users/yufanhuang/Dropbox/orgroam/20230322121329-emacs_links.org" "/Users/yufanhuang/Dropbox/orgroam/20230322221643-scholars_follow.org" "/Users/yufanhuang/Dropbox/orgroam/20230326190738-literature_review_about_burer_monterio_method.org" "/Users/yufanhuang/Dropbox/orgroam/20230327160208-tutorials.org" "/Users/yufanhuang/Dropbox/orgroam/20230329160644-meeting_notes.org" "/Users/yufanhuang/Dropbox/orgroam/20230330113219-shoplist.org" "/Users/yufanhuang/Dropbox/orgroam/20230331110702-julia.org" "/Users/yufanhuang/Dropbox/orgroam/20230406110622-robots_that_learn_and_adapt.org" "/Users/yufanhuang/Dropbox/orgroam/20230412173647-manifold_optimization.org" "/Users/yufanhuang/Dropbox/orgroam/20230423160313-julia_notes.org" "/Users/yufanhuang/Dropbox/orgroam/20230501180303-computational_topology_for_data_analysis_final_topics.org" "/Users/yufanhuang/Dropbox/orgroam/20230503150501-meeting_notes.org" "/Users/yufanhuang/Dropbox/orgroam/20230516093823-mmls2023.org" "/Users/yufanhuang/Dropbox/orgroam/20230525141608-nonlinear_perron_frobenius_theorems_for_nonnegative_tensors.org" "/Users/yufanhuang/Dropbox/orgroam/20230527084407-talks.org" "/Users/yufanhuang/Dropbox/orgroam/20230527214415-notes_on_sdplr_1_03_beta.org" "/Users/yufanhuang/Dropbox/orgroam/20230607102452-influence_maximization_notes.org" "/Users/yufanhuang/Dropbox/orgroam/20230808144914-git.org" "/Users/yufanhuang/Dropbox/orgroam/20230810085218-use_package.org" "/Users/yufanhuang/Dropbox/orgroam/20231017110940-feedback_on_logpagerank_embedding_video.org" "/Users/yufanhuang/Dropbox/orgroam/20231208193449-leetcode.org" "/Users/yufanhuang/Dropbox/orgroam/20231209113702-interview.org" "/Users/yufanhuang/Dropbox/orgroam/20231209115756-org_clock.org" "/Users/yufanhuang/Dropbox/orgroam/20231210104845-trip.org" "/Users/yufanhuang/Dropbox/orgroam/20231210123911-minimal_example_for_org_roam_bibtex.org" "/Users/yufanhuang/Dropbox/orgroam/20231210212834-yasnippt.org" "/Users/yufanhuang/Dropbox/orgroam/20231211141121-group_meeting.org" "/Users/yufanhuang/Dropbox/orgroam/20231211183712-markdown.org" "/Users/yufanhuang/Dropbox/orgroam/20231212160448-regex.org" "/Users/yufanhuang/Dropbox/orgroam/20231212222234-practical_common_lisp.org" "/Users/yufanhuang/Dropbox/orgroam/20231219115929-vim_movement.org" "/Users/yufanhuang/Dropbox/orgroam/20231226113921-documentation.org" "/Users/yufanhuang/Dropbox/orgroam/20231228164052-github_token.org" "/Users/yufanhuang/Dropbox/orgroam/20231229110928-vpn.org" "/Users/yufanhuang/Dropbox/orgroam/20231230124944-jupyter_notebook_to_pdf.org" "/Users/yufanhuang/Dropbox/orgroam/20240102151529-an_introduction_to_programming_in_emacs_lisp.org" "/Users/yufanhuang/Dropbox/orgroam/20240105173612-algorithmic_spectral_graph_theory_boot_camp.org" "/Users/yufanhuang/Dropbox/orgroam/BMNonlinear2003.org" "/Users/yufanhuang/Dropbox/orgroam/ZZG+Social2019.org" "/Users/yufanhuang/Dropbox/orgroam/a_scalable_frank_wolfe_based_algorithm_for_the_max_cut_sdp.org" "/Users/yufanhuang/Dropbox/orgroam/a_survey_of_numerical_methods_for_nonlinear_semidefinite_programming.org" "/Users/yufanhuang/Dropbox/orgroam/augmented_lagrangian.org" "/Users/yufanhuang/Dropbox/orgroam/barvinok_pataki.org" "/Users/yufanhuang/Dropbox/orgroam/book.org" "/Users/yufanhuang/Dropbox/orgroam/bregman_divergence.org" "/Users/yufanhuang/Dropbox/orgroam/constrained_optimization.org" "/Users/yufanhuang/Dropbox/orgroam/convex_optimization.org" "/Users/yufanhuang/Dropbox/orgroam/correlation_clustering.org" "/Users/yufanhuang/Dropbox/orgroam/curvature_constant.org" "/Users/yufanhuang/Dropbox/orgroam/determinant.org" "/Users/yufanhuang/Dropbox/orgroam/diameter.org" "/Users/yufanhuang/Dropbox/orgroam/differentiable.org" "/Users/yufanhuang/Dropbox/orgroam/eigenvalue_decomposition.org" "/Users/yufanhuang/Dropbox/orgroam/estimating_the_largest_eigenvalue_by_the_power_and_lanczos_algorithms_with_a_random_start.org" "/Users/yufanhuang/Dropbox/orgroam/four_cheeger_type_inequalities_for_graph_partitioning_algorithms.org" "/Users/yufanhuang/Dropbox/orgroam/frank_wolfe_conditional_gradient_method.org" "/Users/yufanhuang/Dropbox/orgroam/gershgorin_circle_theorem.org" "/Users/yufanhuang/Dropbox/orgroam/hermitian.org" "/Users/yufanhuang/Dropbox/orgroam/integer_program.org" "/Users/yufanhuang/Dropbox/orgroam/lagrangian_dual.org" "/Users/yufanhuang/Dropbox/orgroam/linear_sieve.org" "/Users/yufanhuang/Dropbox/orgroam/lipschitz.org" "/Users/yufanhuang/Dropbox/orgroam/markov_chain.org" "/Users/yufanhuang/Dropbox/orgroam/max_cut.org" "/Users/yufanhuang/Dropbox/orgroam/max_k_cut.org" "/Users/yufanhuang/Dropbox/orgroam/memory_efficient_approximation_algorithms_for_max_k_cut_and_correlation_clustering.org" "/Users/yufanhuang/Dropbox/orgroam/memory_efficient_structured_convex_optimization_via_extreme_point_sampling.org" "/Users/yufanhuang/Dropbox/orgroam/norm.org" "/Users/yufanhuang/Dropbox/orgroam/nystrom_sketch.org" "/Users/yufanhuang/Dropbox/orgroam/optimality_conditions_for_general_constrained_optimization.org" "/Users/yufanhuang/Dropbox/orgroam/project_euler.org" "/Users/yufanhuang/Dropbox/orgroam/projector.org" "/Users/yufanhuang/Dropbox/orgroam/pseudo_inverse.org" "/Users/yufanhuang/Dropbox/orgroam/pseudoforest.org" "/Users/yufanhuang/Dropbox/orgroam/random_walk.org" "/Users/yufanhuang/Dropbox/orgroam/revisiting_frank_wolfe_projection_free_sparse_convex_optimization.org" "/Users/yufanhuang/Dropbox/orgroam/schatten_p_norms.org" "/Users/yufanhuang/Dropbox/orgroam/semidefinite_program.org" "/Users/yufanhuang/Dropbox/orgroam/semidefinite_programming_hierarchies.org" "/Users/yufanhuang/Dropbox/orgroam/sketching.org" "/Users/yufanhuang/Dropbox/orgroam/sparse_approximate_solutions_to_semidefinite_programs.org" "/Users/yufanhuang/Dropbox/orgroam/stirling_formula.org" "/Users/yufanhuang/Dropbox/orgroam/taylor_expansion.org" "/Users/yufanhuang/Dropbox/orgroam/techniques_in_optimizations_and_sampling.org" "/Users/yufanhuang/Dropbox/orgroam/test1.org" "/Users/yufanhuang/Dropbox/orgroam/topological_structure_of_complex_predictions.org" "/Users/yufanhuang/Dropbox/orgroam/understanding_regularized_spectral_clustering_via_graph_conductance.org") nil nil "Customized with use-package org")
 '(org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "open -a \"Google Chrome\" %s")))
 '(org-format-latex-options
   '(:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\["))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
