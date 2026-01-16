;; Custom project root detection for monorepos
;; Override default project.el behavior to use Gemfile as project root

(defun my/project-find-gemfile-root (dir)
  "Find project root by looking for Gemfile in DIR or its parents.
Returns a cons cell (project-root . 'gemfile) if found, nil otherwise."
  (let ((root (locate-dominating-file dir "Gemfile")))
    (when root
      (cons 'transient root))))

;; Add our custom project finder to the beginning of the list
;; This gives it higher priority than the default VC (git) finder
(add-hook 'project-find-functions #'my/project-find-gemfile-root)

;; Optional: You can also remove the default VC finder entirely if you want
;; to completely disable git-based project detection
;; (setq project-find-functions (delq #'project-try-vc project-find-functions))
