;;; ~/dotfiles/emacs/.config/doom/config.el -*- lexical-binding: t; -*-

;; Doom

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
      (:prefix "a" :desc "Apps"
        :desc "Email" :nv "e" #'=email
        :desc "Processes" :nv "p" #'helm-list-emacs-process)
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


;; Org
(map! :prefix "C-c"
      :desc "Org store link" :nv "l" #'org-store-link)


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


;; Email
(set-email-account!
 "Cisco"
  '((mu4e-sent-folder        . "/Cisco/Sent Items")
    (mu4e-drafts-folder      . "/Cisco/Drafts")
    (mu4e-trash-folder       . "/Cisco/Trash")
    (mu4e-refile-folder      . "/Cisco/All Mail")
    (smtpmail-smtp-user      . "mmyrseth@cisco.com")
    (user-mail-address       . "mmyrseth@cisco.com")
    (mu4e-compose-signature  . " - Martin"))
  t)


;; mu4e
(when (not (featurep 'org-mu4e))
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e"))


(after! mu4e
  (require 'org-mu4e))


;; PureScript
(setq psc-ide-use-npm-bin t)
