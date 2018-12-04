;;; completion/lsp/config.el -*- lexical-binding: t; -*-

(def-package! lsp-mode
  :commands (lsp-mode lsp-define-stdio-client))

(def-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (set-lookup-handlers! 'lsp-ui-mode
                        :definition #'lsp-ui-peek-find-definitions
                        :references #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t))

(def-package! company-lsp
  :after lsp-mode
  :config
  (set-company-backend! 'lsp-mode 'company-lsp)
  (setq company-lsp-enable-recompletion t))

(def-package! cquery
  :when (featurep! +cpp)
  :hook ((c-mode c++-mode objc-mode) . +setup-cquery)
  :init
  (setq cquery-extra-init-params '(:index (:comments 2)
                                          :cacheFormat "json"
                                          :completion (:detailedLabel t))
        cquery-sem-highlight-method 'overlay) ;; set to 'font-lock if highlighting slowly
  (defun +setup-cquery ()
    (setq-local company-transformers nil)
    (setq-local company-lsp-cache-candidates nil)
    (condition-case nil
        (lsp-cquery-enable)
      (user-error nil))))
