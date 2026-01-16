;; Completely disable treesit-auto for Ruby to avoid conflicts
;; We want to use traditional ruby-mode, not ruby-ts-mode
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  ;; Remove ruby from treesit-auto languages
  (setq treesit-auto-langs (delete 'ruby treesit-auto-langs))

  ;; Remove all ruby-ts-mode associations from auto-mode-alist
  (setq auto-mode-alist
        (cl-remove-if (lambda (pair)
                        (eq (cdr pair) 'ruby-ts-mode))
                      auto-mode-alist))

  ;; Ensure we use ruby-mode for all Ruby files
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rbi\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile\\.lock\\'" . ruby-mode))

  ;; Add terraform mode for .tf files
  (add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))

  (global-treesit-auto-mode))

(use-package json-ts-mode
  :mode "\\.ejson\\'") ;; JSON mode for .ejson files
