;; -*- no-byte-compile: t; -*-
;;; completion/lsp/packages.el

(when (package! lsp-mode)
  (package! lsp-ui)
  (package! company-lsp)

  (when (featurep! +javascript)
    (package! lsp-typescript
      :recipe (:fetcher
               github
               :repo "emacs-lsp/lsp-javascript"
               :files ("lsp-typescript.el")))
    (package! tide :disable t))

  (when (featurep! +go)
    (package! lsp-go))

  (when (featurep! +python)
    (package! anaconda-mode :disable t))

  (when (featurep! +rust)
    (package! lsp-rust)
    (package! racer :disable t))

  (when (featurep! +css)
    (package! lsp-css
      :recipe (:fetcher
               github
               :repo "emacs-lsp/lsp-css")))

  (when (featurep! +cpp)
    (package! cquery)
    (package! rtags :disable t))

  (when (featurep! +ocaml)
    (package! lsp-ocaml))

  (when (featurep! +java)
    (package! lsp-intellij))

  (when (featurep! +sh)
    (package! company-shell :disable t)))
