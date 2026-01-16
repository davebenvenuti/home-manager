;; From https://github.com/minad/vertico/blob/main/README.md

;; Enable Vertico.
(use-package vertico
  :config
  (require 'vertico-buffer)
  (require 'vertico-directory)
  (require 'vertico-flat)
  (require 'vertico-indexed)
  (require 'vertico-mouse)
  (require 'vertico-quick)
  (require 'vertico-repeat)
  (require 'vertico-reverse)
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 10) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode)
  :config
  ;; Custom function for TAB behavior
  (defun my/vertico-tab-or-next ()
    "Complete if only one candidate, otherwise move to next candidate."
    (interactive)
    (if (= (length vertico--candidates) 1)
        (minibuffer-complete)
      (vertico-next)))
  
  ;; Make TAB use custom behavior
  ;; Unbind any existing TAB bindings that might interfere
  (keymap-unset vertico-map "TAB")
  (keymap-set vertico-map "TAB" #'my/vertico-tab-or-next)
  ;; Optionally, you can also bind S-TAB to move to previous candidate
  (keymap-set vertico-map "<backtab>" #'vertico-previous))

(use-package vertico-directory
  :straight nil
  :ensure nil
  :after vertico)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))
