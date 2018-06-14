;;; private/myme-config/config.el -*- lexical-binding: t; -*-

(setq
 ;; Look-and-feel
 doom-font (font-spec :family "DejaVu Sans Mono" :size 12)

 ;; Dired
 dired-dwim-target t)

;; Bindings
(map! (:after company
        :map company-mode-map
        "C-/" #'counsel-company))

(map! :leader
      (:desc "theme" :prefix "T"
        :desc "Switch theme" :nv "T" #'counsel-load-theme))
