(use-package kotlin-mode
  :ensure t    ;; Install the package if not already installed
  :defer t     ;; Defer loading until needed
  :mode "\\.kt\\'")

(use-package rust-mode
  :straight t) ;; Rust mode

(use-package protobuf-mode
  :straight t
  :mode "\\.proto\\'") ;; Protocol Buffers mode

(use-package bazel
  :straight (:host github :repo "bazelbuild/emacs-bazel-mode" :files ("bazel.el"))) ;; Bazel mode

(use-package terraform-mode
  :ensure t)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))
