;;; ~/dotfiles/emacs/.config/doom/config.el -*- lexical-binding: t; -*-

;; Doom

(set-popup-rules!
  '(("^\\*"  :slot 1 :vslot -1 :select t)
    ("^ \\*" :slot 1 :vslot -1 :size +popup-shrink-to-fit)
    ("^\\*Async" :slot 1 :vslot -1 :ttl nil)
    ("^\\*Compil\\(?:ation\\|e-Log\\)" :size 0.3 :ttl nil)
    ("^\\*Man " :side right :size 0.4 :vslot -6 :ttl 0 :quit t :select t)))

;; Transparency
(set-frame-parameter (selected-frame) 'alpha 95)
(add-to-list 'default-frame-alist '(alpha  95))

(setq-default
 ;; Look-and-feel
 doom-font (font-spec :family "DejaVu Sans Mono" :size 12)

 ;; Avy
 avy-all-windows 'all-frames

 ;; Line numbers
 display-line-numbers-type 'relative

 ;; Dired
 dired-dwim-target t

 ;; Banner
 +doom-dashboard-banner-dir (concat doom-private-dir "banners/")
 +doom-dashboard-banner-file "rocket-ship.svg"

 ;; Workspaces
 +workspaces-main "default"
 +workspaces-switch-project-function #'ignore

 ;; Info
 Info-additional-directory-list '("~/.cache/info"))

;; Ace window
(after! ace-window
  (setq aw-scope 'global))

;; Jump bindings
(after! evil
  (map! :leader
        (:desc "jump" :prefix "j"
          :desc "Jump to current clock" :nv "C" #'org-clock-goto
          :desc "Jump to character" :nv "c" #'evil-avy-goto-char
          :desc "Jump to function" :nv "f" #'find-function
          :desc "Jump to line" :nv "l" #'evil-avy-goto-line
          :desc "Jump to variable" :nv "v" #'find-variable)))


;; Custom leader bindings
(map!
 (:prefix "C-c i"
   (:desc "Insert today's date" "d" #'myme/insert-date))
 (:leader
   (:prefix "F" :desc "Frame"
     :desc "Delete frame" :nv "d" #'delete-frame
     :desc "New frame" :nv "n" #'make-frame)
   (:prefix "a" :desc "Apps"
     :desc "Email" :nv "e" #'=mu4e
     :desc "Processes" :nv "p" #'counsel-list-processes
     (:prefix "s" :desc "Shells"
       :desc "eshell" :nv "e" #'eshell
       :desc "eshell" :nv "E" #'projectile-run-eshell)
     (:after helm
       :desc "helm-top" :nv "t" #'helm-top))
   (:prefix "b"
     :desc "Rename buffer" :nv "r" #'rename-buffer)
   (:prefix "c"
     :desc "Compile" :nv "c" #'compile
     :desc "Compile" :nv "k" #'kill-compilation)
   (:prefix "p"
     :desc "Open dired in project" :nv "d" #'projectile-dired
     :desc "Run async cmd in project" :nv "&" #'projectile-run-async-shell-command-in-root
     :desc "Test project" :nv "t" #'projectile-test-project)
   (:prefix "q"
     :desc "Kill emacs" :nv "k" #'save-buffers-kill-emacs)
   (:desc "theme" :prefix "T"
     :desc "Switch theme" :nv "T" #'load-theme)))

;; Email
(after! mu4e
  (map!
   (:mode mu4e-main-mode
     :desc "Update index" :nv "U" #'mu4e-update-index)
   (:mode mu4e-view-mode
     :desc "Fill long lines" :nv "M-Q" #'mu4e-view-fill-long-lines))
  (setq mu4e-compose-mode-hook '(org-mu4e-compose-org-mode)
        mu4e-compose-format-flowed nil
        org-mu4e-convert-to-html nil)
  (advice-add #'org~mu4e-mime-switch-headers-or-body
              :after
              (lambda ()
                (if (eq major-mode 'org-mode) (setq fill-column 66)))))

;; Links
(setq ace-link-fallback-function
      (lambda ()
        "Add ace-link functions for additional modes."
        (interactive)
        (cond ((eq major-mode 'mu4e-view-mode)
               (ace-link-mu4e))
              ((eq major-mode 'Man-mode)
               (ace-link-woman))
              (t
               (myme/avy-open-url)))))

;; C/C++
(setq my-cc-style
      '("cc-mode"
        (c-offsets-alist . (
                            (innamespace . [0])
                            (inline-open . 0)))))
(c-add-style "my-cc-style" my-cc-style)
(setq
 c-default-style
 (quote
  ((c-mode . "my-cc-style")
   (c++-mode . "my-cc-style")
   (java-mode . "java")
   (awk-mode . "awk")
   (other . "gnu"))))

;; LSP
(after! lsp-mode
  (map!
   (:mode lsp-mode
     (:leader
       :desc "LSP execute code action" :nv "ca" #'lsp-execute-code-action))))

;; Code
(setq
 ;; JavaScript
 js-indent-level 2
 js-switch-indent-offset 2
 js2-basic-offset 2
 js2-mode-show-parse-errors nil
 js2-mode-show-strict-warnings nil
 js2-strict-missing-semi-warning nil
 js2-strict-trailing-comma-warning nil

 ;; TypeScript
 typescript-indent-level 2

 ;; Web mode
 web-mode-code-indent-offset 2)

;; TypeScript file assoc
(add-to-list 'auto-mode-alist '("\\.tsx$" . typescript-mode))

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

;; Direnv patches
(after! direnv
  (defadvice! +direnv-update-async-shell-command-a (command &optional output-buffer _error-buffer)
    :before #'shell-command
    (when-let ((match (string-match "[ \t]*&[ \t]*\\'" command))
               (directory-name direnv--active-directory))
      (with-current-buffer (get-buffer-create (or output-buffer "*Async Shell Command*"))
        (direnv-update-directory-environment directory-name)))))

;; PureScript
(setq psc-ide-use-npm-bin t)
