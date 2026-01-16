(use-package diff-hl
  :hook (prog-mode . diff-hl-mode)
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  (setq diff-hl-margin-symbols-alist
        '((insert . "+")
          (delete . "-")
          (change . "M")
          (unknown . "?")
          (ignored . "i")))

  ;; Enable on-the-fly diff highlighting for unsaved changes
  ;; This shows indicators for modified lines even before saving
  (diff-hl-flydiff-mode 1)

  ;; Adjust delay if you want faster/slower updates (default: 0.3 seconds)
  ;; (setq diff-hl-flydiff-delay 0.3)

  ;; Example of how to customize the color scheme for diff-hl:
  ;; (custom-set-faces
  ;;  '(diff-hl-insert ((t (:foreground "green" :background "green" :box t))))
  ;;  '(diff-hl-delete ((t (:foreground "red" :background "red" :box t))))
  ;;  '(diff-hl-change ((t (:foreground "blue" :background "blue" :box t)))))
  )

(defun my/github-permalink ()
  "Generate a permanent GitHub URL for the current file.  Copies the URL to the kill ring and returns it."
  (interactive)
  (let* ((git-root (locate-dominating-file default-directory ".git"))
         (filepath (if git-root
                       (file-relative-name buffer-file-name git-root)
                     (error "Not in a git repository")))
         (branch (shell-command-to-string
                  "git rev-parse --abbrev-ref HEAD 2>/dev/null | tr -d '\n'"))
         (commit (shell-command-to-string
                  "git rev-parse HEAD 2>/dev/null | tr -d '\n'"))
         (remote-url (shell-command-to-string
                      "git config --get remote.origin.url | tr -d '\n'"))
         (github-url (replace-regexp-in-string
                      "\\(git@github.com:\\|https://github.com/\\|.git$\\)" "" remote-url))
         (line-reference
          (if (use-region-p)
              (let ((start-line (line-number-at-pos (region-beginning)))
                    (end-line (line-number-at-pos (region-end))))
                ;; If selection ends at line start, don't include that line
                (when (and (> end-line start-line)
                           (= (region-end) (line-beginning-position)))
                  (setq end-line (1- end-line)))
                (if (= start-line end-line)
                    (format "#L%d" start-line)
                  (format "#L%d-L%d" start-line end-line)))
            (format "#L%d" (line-number-at-pos))))
         (permalink (format "https://github.com/%s/blob/%s/%s%s"
                           github-url commit filepath line-reference)))
    ;; Add to kill-ring
    (kill-new permalink)

    ;; Also copy to system clipboard using pbcopy
    (with-temp-buffer
      (insert permalink)
      (shell-command-on-region (point-min) (point-max) "pbcopy" nil nil nil t))

    (message "GitHub permalink copied: %s" permalink)
    permalink))

(global-set-key (kbd "C-c g") 'my/github-permalink)

(use-package smerge-mode
  :hook (magit-diff-mode . smerge-mode) ; Activates smerge-mode in Magit diff buffers
  :bind (("C-c ^ m" . smerge-use-mine) ; Example keybinding to keep "ours" (HEAD)
         ("C-c ^ o" . smerge-use-other) ; Example keybinding to keep "theirs" (incoming)
         ("C-c ^ n" . smerge-next-conflict)) ; Example keybinding to next conflict
  :config
  (smerge-set-key-bindings) ; Sets up standard smerge keys within the mode
  (setq smerge-mode-hook #'(lambda () (message "Smerge mode activated for conflicts!"))))
