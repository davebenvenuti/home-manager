;; Set UTF-8 as the default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; straight.el (use-package extension)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Https://github.com/radian-software/straight.el/issues/1146#issuecomment-2227133737
(setq straight-built-in-pseudo-packages '(emacs project xref eglot))
(setq straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Load all *.el files in ~/.emacs.d in lexographical order.  Also note that
;; *.private.el is git ignored.
(let ((config-files (directory-files user-emacs-directory t "^[^#].*\\.el$")))
  (setq config-files (cl-remove-if (lambda (file)
                                     (string= (file-name-nondirectory file) "init.el"))
                                   config-files))
  (dolist (file (sort config-files #'string<))
    (load file)))
(put 'upcase-region 'disabled nil)
