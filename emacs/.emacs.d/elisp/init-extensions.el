;;; package --- Here be extensions
;;; Commentary:
;;; Code:

;;
;; General.el - Key bindings
;;

(use-package general :ensure t)


;;
;; Completion: Avy, Company, Ivy/Counsel/Swiper
;;

(use-package avy
  :ensure t
  :general (
    "C-c SPC" 'avy-goto-char
    )
  )


(use-package company
  :diminish company-mode
  :ensure t
  :general (
    :keymaps 'company-active-map
    "C-j" 'company-select-next
    "C-k" 'company-select-previous
    "C-l" 'company-complete-selection
    )

  :config

  (add-hook 'after-init-hook 'global-company-mode)
  )


(use-package company-quickhelp
  :ensure t
  :general (
    :keymaps 'company-active-map
    "M-h" 'company-quickhelp-manual-begin
    )

  :config
  (setq company-quickhelp-delay nil)
  (company-quickhelp-mode 1)
  )


(use-package counsel
  :ensure t
  :general
    ("M-x" 'counsel-M-x)
    (:prefix "C-x"
      "C-m" 'counsel-M-x
      "C-f" 'counsel-find-file
      "ck" 'counsel-yank-pop
      )
  )


(use-package counsel-projectile
  :ensure t
  :general
    (:prefix "C-x"
      "v" 'counsel-projectile
      "cp" 'counsel-projectile-ag
      )

  :config
  (counsel-projectile-on)
  )


(use-package ivy
  :diminish ivy-mode
  :ensure t
  :general
    (:prefix "C-x"
      "C-r" 'ivy-resume
      )
    (:keymaps 'read-expression-map
      "C-r" 'counsel-expression-history
      )

  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil)
  )


(use-package swiper
  :ensure t
  :general
    ("C-s" 'swiper)
  )


;;
;; Dashboard
;;

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents  . 5)
                         (bookmarks . 5)
                         (projects . 5)))
  )


(use-package ediff
  :ensure t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w")
  )


(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "PYTHONPATH")
    (exec-path-from-shell-initialize)
    )
  )


(use-package expand-region
  :ensure t
  :general (
    "C-=" 'er/expand-region
    )
  )


(use-package flycheck
  :diminish flycheck-mode
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))


;;
;; Linum
;;

(use-package linum
  :ensure t
  :config

  (setq linum-format " %3d ")
  (global-linum-mode nil)

  (use-package hlinum
    :ensure t
    :config
    (hlinum-activate))

  (use-package linum-relative
    :ensure t
    :config
    (setq linum-relative-current-symbol "")
    (linum-relative-on)
    )
  )


;;
;; Magit / Git
;;

(use-package magit
  :ensure t
  :config

  (setq magit-completing-read-function 'ivy-completing-read)

  :general (
    :prefix "C-x"
    "gs" 'magit-status
    "gx" 'magit-checkout
    "gc" 'magit-commit
    "gp" 'magit-push
    "gu" 'magit-pull
    "ge" 'magit-ediff-resolve
    "gr" 'magit-rebase-interactive
    )
  )


(use-package magit-popup
  :ensure t)


(use-package multiple-cursors
  :ensure t
  :general (
    "C-S-c C-S-c" 'mc/edit-lines
    "C->" 'mc/mark-next-like-this
    "C-<" 'mc/mark-previous-like-this
    "C-c C->" 'mc/mark-all-like-this
    )
  )


(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'arrow
        neotree-smart-optn t
        neo-window-fixed-size nil)
  ;; Disable linum for neotree
  ;; (add-hook 'neo-after-create-hook 'disable-neotree-hook))
  )


;;
;; Org mode
;;

(use-package org
  :ensure t
  :general (
    :prefix "C-c"
    "l" 'org-store-link
    "a" 'org-agenda
    )
  )

  :config
  (setq org-directory "~/Documents/org"
        org-default-notes-file (concat org-directory "/todo.org"))


(use-package org-projectile
  :ensure t
  :config
  (org-projectile:per-repo)
  (setq org-projectile:per-repo-filename "todo.org"
	org-agenda-files (append org-agenda-files (org-projectile:todo-files))))


(use-package org-bullets
  :ensure t
  :config
  (setq org-hide-leading-stars t)
  (add-hook 'org-mode-hook
            (lambda ()
              (org-bullets-mode t))))


(use-package page-break-lines
  :ensure t)


;;
;; Perspectives / Projects / Recents
;;

(use-package perspective
  :ensure t
  :config
  (persp-mode 1))


(use-package persp-projectile
  :ensure t)


(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" temp-dir))

  (setq projectile-completion-system 'ivy)

  (projectile-global-mode))


(use-package recentf
  :ensure t
  :config
  (setq recentf-save-file (recentf-expand-file-name (concat temp-dir "/recentf")))
  (recentf-mode 1))


;;
;; Smartparens
;;

(use-package smartparens
  :ensure t)


(use-package smex
  :ensure t)


(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  ;; Remember undo history
  (setq
   undo-tree-auto-save-history nil
   undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/"))))
  (global-undo-tree-mode 1))


(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (define-key global-map (kbd "C-h <SPC>") 'which-key-show-top-level)
  (which-key-mode))


(use-package wgrep
  :ensure t)


;;
;; Snippets
;;

(use-package yasnippet
  :diminish yas-minor-mode
  :ensure t
  :config
  (yas-global-mode 1))


;;
;; Evil
;;

(use-package evil
  :ensure t
  :init
  (evil-mode 1)

  :general
    ;; Evil leader
    (
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC"

    ;; M-x
    "SPC" 'counsel-M-x

    ;; comments
    "c" '(:ignore t :which-key "comments")
    "ci" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" 'evilnc-copy-and-comment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cr" 'comment-or-uncomment-region
    "cv" 'evilnc-toggle-invert-comment-line-by-line

    ;; buffers
    "b" '(:ignore t :which-key "buffers")
    "bb" 'ivy-switch-buffer
    "bd" 'kill-this-buffer
    "bn" 'next-buffer
    "bp" 'previous-buffer

    ;; errors / flycheck
    "e" '(:ignore t :which-key "errors")
    "ee" 'flycheck-mode
    "en" 'flycheck-next-error
    "ep" 'flycheck-previous-error

    ;; files
    "f" '(:ignore t :which-key "files")
    "ff" 'counsel-find-file
    "fr" 'counsel-recentf

    ;; git
    "g" '(:ignore g :which-key "git/vcs")
    "gs" 'magit-status

    ;; projectile
    "p" '(:ignore p :which-key "projectile")
    "pf" 'projectile-find-file
    "pl" 'projectile-persp-switch-project
    "pp" 'projectile-switch-project

    ;; toggles
    "t" '(:ignore t :which-key "toggles")
    "tF" 'toggle-frame-fullscreen
    "tn" 'linum-mode
    "tr" 'linum-relative-toggle
    )

    ;; Evil org mode
    (
    :keymaps 'org-mode-map
    :states '(normal visual)

    "gh" 'outline-up-heading
    "gj" 'org-forward-heading-same-level
    "gk" 'org-backward-heading-same-level
    "gl" 'outline-next-visible-heading

    "C-]" 'org-open-at-point

    "M-h" 'org-metaleft
    "M-j" 'org-metadown
    "M-k" 'org-metaup
    "M-l" 'org-metaright

    "M-H" 'org-shiftmetaleft
    "M-J" 'org-shiftmetadown
    "M-K" 'org-shiftmetaup
    "M-L" 'org-shiftmetaright
      )

  :config

  (use-package evil-magit
    :ensure t
    :config
    (evil-magit-init))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-matchit :ensure t)
  (use-package evil-nerd-commenter :ensure t)
  )


(provide 'init-extensions)
;;; init-extensions.el ends here
