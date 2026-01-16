(defun my/env-var-to-list (var-name &optional delimiter)
  "Convert a delimited environment variable specified by VAR-NAME to a list of strings.
An optional DELIMITER can be provided, defaulting to a comma."
  (let ((env-value (getenv var-name))
        (delim (or delimiter ",")))
    (when env-value
      ;; Split the string by the specified delimiter and trim each element
      (mapcar #'string-trim (split-string env-value delim t)))))

(defun my/env-var-set-p (var)
  "Return non-nil if environment variable VAR is set and not empty."
  (let ((value (getenv var)))
    (and value (not (string-empty-p value)))))

(defun my/setup-gptel ()
  "Setup gptel for hosted deepseek or a configured OpenAI proxy."
  (interactive)
  (when (my/env-var-set-p "GPTEL_DEEPSEEK_API_KEY")
    (message "[gptel] Setting up DeepSeek")
    (setq my/deepseek-api-key (getenv "GPTEL_DEEPSEEK_API_KEY"))

    (gptel-make-deepseek "DeepSeek Chat"
                         :stream t
                         :key my/deepseek-api-key)

    (setq gptel-model 'deepseek-coder
          gptel-backend (gptel-make-openai "DeepSeek Coder"
                          :stream t
                          :host "api.deepseek.com"
                          :endpoint "/chat/completions"
                          :key my/deepseek-api-key
                          :models (list "deepseek-coder" "deepseek-reasoner"))
          gptel-max-tokens 8192))

  (when (my/env-var-set-p "GPTEL_OPENAI_PROXY_API_KEY")
    (message "[gptel] Setting up OpenAI Proxy")
    (setq my/openai-api-key (getenv "GPTEL_OPENAI_PROXY_API_KEY"))

    (setq gptel-model (intern (getenv "GPTEL_OPENAI_PROXY_DEFAULT_MODEL"))
      gptel-backend
      (gptel-make-openai "OpenAI Proxy"
        :host (getenv "GPTEL_OPENAI_PROXY_HOST")
        :endpoint "/v1/chat/completions"
        :stream t
        :key my/openai-api-key
        :models (my/env-var-to-list "GPTEL_OPENAI_PROXY_MODELS"))
      gptel-max-tokens 16384)))

;; gptel notes
;;
;;  - C-c <ret> sends the message in the `gptel` chat buffer
(use-package gptel
  :straight t
  :config
  (my/setup-gptel))

;; old
;; (use-package gptel
;;   :straight t
;;   :config
;;   (setq gptel-model (intern (getenv "GPTEL_DEFAULT_MODEL"))
;;       gptel-backend
;;       (gptel-make-openai (or (getenv "GPTEL_NAME") "OpenAI")
;;         :host (getenv "GPTEL_API_HOST")
;;         :endpoint (getenv "GPTEL_API_ENDPOINT")
;;         :stream t
;;         :key (getenv "GPTEL_API_KEY")
;;         :models (env-var-to-list "GPTEL_MODELS"))
;;       gptel-max-tokens (string-to-number (or (getenv "GPTEL_MAX_TOKENS") "16384"))))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :config
  (setq copilot-indent-offset-warning-disable t)
  (add-hook 'prog-mode-hook 'copilot-mode)

  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-c n") 'copilot-next-completion)

  ;; https://github.com/copilot-emacs/copilot.el/issues/312
  (add-to-list 'copilot-indentation-alist '(prog-mode tab-width))
  (add-to-list 'copilot-indentation-alist '(org-mode tab-width)))
