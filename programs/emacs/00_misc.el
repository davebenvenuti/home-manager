(use-package rg
  :straight t) ;; ripgrep

(use-package ag
  :straight t) ;; Silver Searcher (ag) mode

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

;; Set UTF-8 as the default encoding
(dolist (mode '(vterm-mode
                term-mode
                shell-mode
                eshell-mode))
  (add-hook (intern (format "%s-hook" mode))
            (lambda () (display-line-numbers-mode 0))))

;; Set various modes, hooks and variables here
(menu-bar-mode -1)
(xterm-mouse-mode 0)
(column-number-mode 1)
;; This works better than global-display-line-numbers-mode apparently
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(setq-default indent-tabs-mode nil)

(delete-selection-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves/" t)))
(setq lock-file-name-transforms '((".*" "~/.emacs.d/locks/" t)))
(setq native-comp-async-report-warnings-errors 'silent)
