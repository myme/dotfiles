;;; private/myme-config/config.el -*- lexical-binding: t; -*-

;; Popups
(set-popup-rules!
 '(("^ \\*" :slot 1 :vslot -1 :size #'+popup-shrink-to-fit)
   ("^\\*"  :slot 1 :vslot -1 :select t :ttl 0)))

(setq
 ;; Look-and-feel
 doom-font (font-spec :family "DejaVu Sans Mono" :size 12)

 ;; Avy
 avy-all-windows 'all-frames

 ;; Dired
 dired-dwim-target t

 ;; Workspaces
 +workspaces-main "default")

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
      (:prefix "/"
        :desc "Ivy resume" :nv "r" #'ivy-resume)
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
        :desc "Switch theme" :nv "T" #'doom/switch-theme))

(after! ivy
  (ivy-add-actions
   'counsel-projectile-switch-project
   '(("v" magit-status "version control"))))


;; JavaScript
(setq
 js-indent-level 2
 js-switch-indent-offset 2
 js2-basic-offset 2
 js2-mode-show-parse-errors nil
 js2-mode-show-strict-warnings nil
 js2-strict-missing-semi-warning nil
 js2-strict-trailing-comma-warning nil)

(add-hook! js2-mode
  (myme/use-eslint-from-node-modules))
