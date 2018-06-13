;;; private/myme-config/config.el -*- lexical-binding: t; -*-

;; Look-and-feel
(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 12))

;; Bindings
(map! (:after company
        :map company-mode-map
        "C-/" #'counsel-company))

(map! :leader
      (:desc "theme" :prefix "T"
        :desc "Switch theme" :nv "T" #'counsel-load-theme))

