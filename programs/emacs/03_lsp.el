(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PATH")
  (exec-path-from-shell-copy-env "GEM_HOME")
  (exec-path-from-shell-copy-env "GEM_PATH")
  (exec-path-from-shell-copy-env "RUBY_VERSION"))



(use-package eglot
  :ensure nil  ;; Use built-in version
  :commands eglot
  :bind (:map eglot-mode-map
         ("M-RET" . eglot-code-actions)
         ("C-c a" . eglot-code-actions-at-point))
  :hook ((python-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         ;; (ruby-mode . eglot-ensure) ruby-lsp keeps crashing on me
         ;; Add other modes as needed
         )
  :config
  ;; Optional configuration
  (setq eglot-autoshutdown t)

  ;; Increase timeout for language server initialization (default is 30 seconds)
  (setq eglot-connect-timeout 120)  ;; 2 minutes for gem installation

  ;; Configure code actions to show test run options
  (setq eglot-ignored-server-capabilities nil) ;; Make sure all capabilities are enabled

  ;; Use 'mode-line' display for code actions to avoid margin conflicts with diff-hl
  ;; This shows the lightbulb indicator in the mode-line at the bottom of the window
  (setq eglot-code-action-indications '(eldoc-hint mode-line))

  ;; Ensure ruby-mode uses ruby-lsp
  (add-to-list 'eglot-server-programs '(ruby-mode . ("ruby-lsp"))))

;; ==============================================================================
;; SOLVED: Eglot code action indicator now uses 'nearby' display
;; ==============================================================================
;;
;; ORIGINAL PROBLEM:
;; When running Emacs in terminal mode with emoji configured to display width 2
;; (via char-width-table in 09_emoji.el), eglot's code action indicator (💡)
;; was causing visual alignment glitches when displayed in the margin.
;;
;; ADDITIONAL PROBLEM:
;; When diff-hl also used margins (diff-hl-margin-mode), both eglot and diff-hl
;; competed for the same margin space on modified lines with code actions,
;; causing display glitches even with correct margin width.
;;
;; SOLUTION:
;; Configured eglot to use 'nearby' display mode (see :config above) instead of
;; 'margin'. This shows the lightbulb indicator right next to the cursor where
;; the code action is available, avoiding all margin conflicts with diff-hl.
;;
;; BENEFITS:
;; - No margin width issues with wide emoji characters
;; - No conflicts between eglot and diff-hl on the same line
;; - Indicator appears exactly where it's relevant (at cursor position)
;; - diff-hl git indicators remain clearly visible in margin
;;
;; ALTERNATIVE SOLUTIONS (if you want to revisit):
;; 1. Move eglot to mode-line: (setq eglot-code-action-indications '(eldoc-hint mode-line))
;; 2. Move diff-hl to right margin or fringes (GUI only)
;; 3. Use ASCII indicator for eglot: (setq eglot-code-action-indicator ">")
;;
;; DIAGNOSTIC FUNCTION (kept for troubleshooting):
;; (defun my/find-compile-commands-dir ()
;;   "Locate the directory containing the 'compile_commands.json' file and print it."
;;   (let ((project-root (locate-dominating-file default-directory "compile_commands.json")))
;;     (if project-root
;;         (progn
;;           (message "Found compile_commands.json in directory: %s" project-root)
;;           project-root)
;;       (message "compile_commands.json not found in any parent directory.")
;;       nil)))

;; Assign the directory to a variable and print it.
;; (setq my-compile-commands-dir (my/find-compile-commands-dir))

;; M-? allows you to navigate references etc
;; (with-eval-after-load 'eglot
;;   (add-hook 'eglot-managed-mode-hook
;;             (lambda ()
;;               (let ((root (locate-dominating-file
;;                            default-directory
;;                            "compile_commands.json")))
;;                 (when root
;;                   (setq default-directory root))))))

;; (use-package lsp-mode
;;   :bind
;;   ("M-RET" . lsp-execute-code-action)
;;   ("C-c a" . lsp-avy-lens) ;; For things like Run Test in Rails unit tests
;;   :hook
;;   ((ruby-mode . lsp-deferred)
;;    (terraform-mode . lsp-deferred))
;;   :config
;;   (setq lsp-disabled-clients '(rubocop-ls sorbet-ls semgrep-ls)))
;; Note: there's also an lsp-enabled-clients
