;;; ~/dotfiles/emacs/.config/doom/config.el -*- lexical-binding: t; -*-

;; Doom

(set-popup-rules!
  '(("^\\*"  :slot 1 :vslot -1 :select t)
    ("^ \\*" :slot 1 :vslot -1 :size +popup-shrink-to-fit)
    ("^\\*Async" :slot 1 :vslot -1 :ttl nil)
    ("^\\*Compil\\(?:ation\\|e-Log\\)" :size 0.3 :ttl nil)
    ("^\\*Man " :side right :size 0.4 :vslot -6 :ttl 0 :quit t :select t)))

;; Transparency
;; (set-frame-parameter (selected-frame) 'alpha 95)
;; (add-to-list 'default-frame-alist '(alpha  95))

(setq-default
 ;; Look-and-feel
 doom-font (font-spec :family "DejaVu Sans Mono" :size 12)

 ;; Avy
 avy-all-windows 'all-frames


 ;; Dired
 dired-dwim-target t

 ;; Banner
 +doom-dashboard-banner-dir (concat doom-private-dir "banners/")
 +doom-dashboard-banner-file "mushroom.png"

 ;; Workspaces
 +workspaces-main "default"
 +workspaces-switch-project-function #'ignore)


;; Ace window
(after! ace-window
  (setq aw-scope 'global))


;; Jump bindings
(after! evil
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
        :desc "Email" :nv "e" #'=email
        :desc "Processes" :nv "p" #'helm-list-emacs-process)
      (:prefix "b"
        :desc "Rename buffer" :nv "r" #'rename-buffer)
      (:prefix "/"
        :desc "Open URL" :n "l" #'ace-link)
      (:prefix "p"
        :desc "Run async cmd in project" :nv "&" #'projectile-run-async-shell-command-in-root
        :desc "Test project" :nv "t" #'projectile-test-project)
      (:prefix "q"
        :desc "Kill emacs" :nv "k" #'save-buffers-kill-emacs)
      (:desc "theme" :prefix "T"
        :desc "Switch theme" :nv "T" #'doom/switch-theme))

(after! mu4e
  (map!
   :mode mu4e-main-mode
   (:desc "Update index" :nv "U" #'mu4e-update-index)
   :mode mu4e-view-mode
   (:desc "Fill long lines" :nv "M-Q" #'mu4e-view-fill-long-lines)))

(setq ace-link-fallback-function #'myme/avy-open-url)


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


;; (after! eslintd-fix
;;   (add-hook 'js2-mode-hook 'eslintd-fix-mode t))


;; (def-package! prettier-js
;;   :config
;;   (progn
;;     (add-hook 'js2-mode-hook 'pretter-js-mode t)))


;; mu4e
(when (not (featurep 'org-mu4e))
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e"))


(after! mu4e
  (require 'org-mu4e))


;; PureScript
(setq psc-ide-use-npm-bin t)
