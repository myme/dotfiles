;;; private/myme-config/config.el -*- lexical-binding: t; -*-

;; Popups
(set! :popups
  '("^ \\*" ((slot . 1) (vslot . -1) (size . +popup-shrink-to-fit)) ((transient)))
  '("^\\*"  ((slot . 1) (vslot . -1)) ((select . t) (transient))))

(setq
 ;; Look-and-feel
 doom-font (font-spec :family "DejaVu Sans Mono" :size 12)

 ;; Avy
 avy-all-windows 'all-frames

 ;; Dired
 dired-dwim-target t)

;; Bindings
(map! (:after company
        :map company-mode-map
        "C-/" #'counsel-company))

(after! ace-window
  (setq aw-scope 'global))

(after! evil
  (require 'evil-collection-avy)
  (map! :leader
        (:desc "jump" :prefix "j"
          :desc "Jump to character" :nv "c" #'evil-avy-goto-char
          :desc "Jump to line" :nv "l" #'evil-avy-goto-line)))

(map! :leader
      (:prefix "p"
        :desc "Run async cmd in project" :nv "&" #'projectile-run-async-shell-command-in-root)
      (:desc "theme" :prefix "T"
        :desc "Switch theme" :nv "T" #'counsel-load-theme))

(after! ivy
  (ivy-add-actions
   'counsel-projectile-switch-project
   '(("v" magit-status "version control"))))

