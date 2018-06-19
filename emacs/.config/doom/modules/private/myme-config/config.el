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
 dired-dwim-target t

 ;; Workspaces
 +workspaces-main "default")

(define-key minibuffer-local-map (kbd "\C-r") #'counsel-minibuffer-history)

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
          :desc "Jump to function" :nv "f" #'find-function
          :desc "Jump to line" :nv "l" #'evil-avy-goto-line
          :desc "Jump to variable" :nv "v" #'find-variable)))

;; Custom leader bindings
(map! :leader
      (:prefix "a" :desc "Apps"
        :desc "Processes" :nv "p" #'counsel-list-processes)
      (:prefix "b"
        :desc "Rename buffer" :nv "r" #'rename-buffer)
      (:prefix "o"
        :desc "Open URL" :nv "u" #'myme/avy-open-url)
      (:prefix "p"
        :desc "Run async cmd in project" :nv "&" #'projectile-run-async-shell-command-in-root)
      (:prefix "q"
        :desc "Kill emacs" :nv "k" #'kill-emacs)
      (:desc "theme" :prefix "T"
        :desc "Switch theme" :nv "T" #'counsel-load-theme))

(after! ivy
  (ivy-add-actions
   'counsel-projectile-switch-project
   '(("v" magit-status "version control"))))

