;;; private/myme-config/config.el -*- lexical-binding: t; -*-

;; Popups
(set-popup-rules!
 '(("^ \\*" ((slot . 1) (vslot . -1) (size . +popup-shrink-to-fit)) ((transient)))
   ("^\\*"  ((slot . 1) (vslot . -1)) ((select . t) (transient)))))

(setq
 ;; Look-and-feel
 doom-font (font-spec :family "DejaVu Sans Mono" :size 12)

 ;; Avy
 avy-all-windows 'all-frames

 ;; Dired
 dired-dwim-target t)

;; Bindings - TODO this doesn't work optimally as selecting a candidate often breaks the existing text
(map! (:after company
        :map company-mode-map
        "C-/" #'counsel-company))

;; Ace window
(after! ace-window
  (setq aw-scope 'global))

;; Jump bindings
(after! evil
  (require 'evil-collection-avy)
  (map! :leader
        (:desc "jump" :prefix "j"
          :desc "Jump to current clock" :nv "C" #'org-clock-jump-to-current-clock
          :desc "Jump to character" :nv "c" #'evil-avy-goto-char
          :desc "Jump to line" :nv "l" #'evil-avy-goto-line)))

;; Custom leader bindings
(map! :leader
      (:prefix "b"
        :desc "Rename buffer" :nv "r" #'rename-buffer)
      (:prefix "o"
        :desc "Open URL" :nv "u" #'myme/avy-open-url)
      (:prefix "p"
        :desc "Run async cmd in project" :nv "&" #'projectile-run-async-shell-command-in-root)
      (:desc "theme" :prefix "T"
        :desc "Switch theme" :nv "T" #'counsel-load-theme))

(after! ivy
  (ivy-add-actions
   'counsel-projectile-switch-project
   '(("v" magit-status "version control"))))

