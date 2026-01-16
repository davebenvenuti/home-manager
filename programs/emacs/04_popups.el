;; Prevent eldoc (and eglot) from resizing the echo area on hover
(setq eldoc-echo-area-use-multiline-p nil)

(add-to-list 'display-buffer-alist
             '("\\*eldoc\\*"
               (display-buffer-pop-up-window)
               (window-width . 60))) ;; Set the width to x columns

;; I used to use the `popper` package here but that wasn't super useful

(use-package shackle
  :straight t
  :config
  ;; Enable shackle
  (shackle-mode 1)

  ;; Configure shackle rules
  (setq shackle-rules
    '(("*Warnings*" :popup nil)))

  ;; Set a default rule for all windows optionally
  ;; (setq shackle-default-rule '(:select nil :inhibit-window-quit t :ignore t))
  )

;; Prevent the *Warnings* buffer from being displayed
;; (add-to-list 'display-buffer-alist
;;              '("^\\*Warnings\\*" . (display-buffer-no-window)))

(add-to-list 'display-buffer-alist
             '("^\\*Warnings\\*"
               (display-buffer-pop-up-window)
               (inhibit-same-window . t)))

;; Individual warning types can be suppressed, too.
;; (setq warning-suppress-types '((some-warning-type)))
