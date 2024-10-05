;; Dictionary
;; I want to use builtin dictionary to search some unknown words.
;; set up builtin dictionary
(setq dictionary-server "dict.org")
(global-set-key (kbd "C-c d s") 'dictionary-search)
(global-set-key (kbd "C-c d d") 'dictionary-lookup-definition)
