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

(def-package! lsp-typescript
  :when (featurep! +javascript)
  :hook ((js2-mode typescript-mode) . lsp-typescript-enable))

(def-package! company-lsp
  :after lsp-mode
  :config
  (set-company-backend! 'lsp-mode 'company-lsp)
  (setq company-lsp-enable-recompletion t))

(def-package! lsp-go
  :when (featurep! +go)
  :hook (go-mode . lsp-go-enable))

(def-package! lsp-css
  :when (featurep! +css)
  :hook ((css-mode less-mode scss-mode) . lsp-css-enable))

(def-package! lsp-rust
  :when (featurep! +rust)
  :hook (rust-mode . lsp-rust-enable)
  :init
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls")))

(def-package! cquery
  :when (featurep! +cpp)
  :hook ((c-mode c++-mode objc-mode) . +setup-cquery)
  :init
  (setq cquery-extra-init-params '(:index (:comments 2)
                                          :cacheFormat "msgpack"
                                          :completion (:detailedLabel t))
        cquery-sem-highlight-method 'overlay) ;; set to 'font-lock if highlighting slowly
  (defun +setup-cquery ()
    (setq-local company-transformers nil)
    (setq-local company-lsp-cache-candidates nil)
    (condition-case nil
        (lsp-cquery-enable)
      (user-error nil))))

(def-package! lsp-ocaml
  :when (featurep! +ocaml)
  :hook ((tuareg-mode reason-mode) . lsp-ocaml-enable))

(def-package! lsp-intellij
  :when (featurep! +java)
  :hook (java-mode . lsp-intellij-enable))


(when (featurep! +python)
  (after! python
    (lsp-define-stdio-client lsp-python "python"
                             #'projectile-project-root
                             '("pyls"))
    (add-hook! python-mode #'lsp-python-enable)))

(when (featurep! +sh)
  (after! sh-script
    (lsp-define-stdio-client lsp-sh
                            "sh"
                            #'projectile-project-root
                            '("bash-language-server" "start"))
    (add-hook 'sh-mode-hook #'lsp-sh-enable)))
