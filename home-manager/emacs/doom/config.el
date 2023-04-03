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


(defun myme/doom-ascii-banner ()
  (let* ((banner
          '(
            "                 ...',;;;;;;,''.."
            "            ..,;;;;;;;;;;;;;;;;;;;;,.."
            "          .;;;;;;;;;;;;;:c:;;;;;;;;;;;;."
            "       .,;;;;;;;;;;;;:looK0kc;;;;;;;;;;;;,."
            "     .,;;;;;;;;;;;;;cooooKKK0o,;;;;;;;;;;;;,."
            "    .;;;;;;;;;;;;;;coolccldk0Ko',;;;;;;;;;;;;."
            "   ,;;;;;;;;;;;;;;:occkKNKOd;d0:.',;;;;;;;;;;;,"
            "  ,;;;;;;;;;;;;;;;lccWMMMNNNX,dO'..',;;;;;;;;;;,"
            " .;;;;;;;;;;;;;;;co:xMMMMNNNNlcKl....',;;;;;;;;;."
            " ;;;;;;;;;;;;;;;;lol:0MMMNNNk'k0O......',;;;;;;;;"
            ";;;;;;;;;;;;;;;;ooolclxkdl:lOKKK'.......',;;;;;;."
            ",;;;;;;;;;;;;;;;:ooooooolO00KKKK0;.........',;;;;,"
            ";;;;;;;;;;;;;;;;dooooooooKKKKKKK0c...........',;;;"
            ",;;;;;;;;;;;;;o0KKxoooooo0KKKK0Oool,...........',,"
            ";;;;;;;;;;;;xKKKKKdooooOk0KKKOooooo:............."
            " ;;;;;;;;;;;kKKKKKK0oooOKdk0KKdooooooc..........."
            " .;;;;;;;;;dKKKKKKKKoodKKddOKKoooooooo;.........."
            "  ,;;;;;;;;KKKKKKKKOooxKKddkKKxoooooool........."
            "   ,;;;;;;cKKKKKKx:'..cKKddl...,:looooo'......."
            "    .;;;;;cKKKxc'.....'KKdd:......,cloo'......"
            "     .,;;;;xc'.........oKdo..........,c....."
            "       .,;;;'...........xo'..............."
            "          .;;;'........................."
            "            ..,;'...................."
            "                 ..............."
            ""
            "                    E M A C S"
            )
          )
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))


(setq-default
 ;; Look-and-feel
 ;; doom-font (font-spec :family "DejaVu Sans Mono for Powerline" :size 14)
 doom-font (font-spec :family @doomFontFamily@ :size @doomFontSize@)
 doom-theme '@doomTheme@

 ;; Avy
 avy-all-windows 'all-frames

 ;; Line numbers
 display-line-numbers-type 'relative

 ;; Dired
 dired-dwim-target t

 ;; Banner
 +doom-dashboard-banner-dir (concat doom-private-dir "banners/")
 +doom-dashboard-banner-file "rocket-ship.svg"
 +doom-dashboard-ascii-banner-fn #'myme/doom-ascii-banner

 ;; Workspaces
 +workspaces-main "default"
 +workspaces-switch-project-function #'find-file

 ;; Info
 Info-additional-directory-list '("~/.nix-profile/share/info")

 ;; Reload buffers on file updates
 global-auto-revert-mode t)

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
  (:desc "Insert today's date" "d" #'myme/insert-date)
  (:desc "Insert current timestamp" "t" #'myme/insert-timestamp))
 (:prefix "g z"
  (:desc "Skip and goto next match" :nv "s" #'evil-mc-skip-and-goto-next-match)
  (:desc "Skip and goto prev match" :nv "S" #'evil-mc-skip-and-goto-prev-match))
 (:leader
  (:prefix "F" :desc "Frame"
   :desc "Delete frame" :nv "d" #'delete-frame
   :desc "New frame" :nv "n" #'make-frame)
  (:prefix "b"
   :desc "Rename buffer" :nv "r" #'rename-buffer)
  (:prefix "c"
   :desc "Compile" :nv "c" #'compile
   :desc "Compile" :nv "k" #'kill-compilation)
  (:prefix "d"
   :desc "Ediff buffers" :nv "b" #'ediff-buffers)
  (:prefix "p"
   :desc "Open dired in project" :nv "d" #'projectile-dired
   :desc "Run async cmd in project" :nv "&" #'projectile-run-async-shell-command-in-root
   :desc "Test project" :nv "t" #'projectile-test-project)
  (:prefix "q"
   :desc "Kill emacs" :nv "k" #'save-buffers-kill-emacs)
  (:prefix "s"
   :desc "Consult complex commands" :nv "c" #'consult-complex-command)
  (:desc "theme" :prefix "T"
   :desc "Switch theme" :nv "T" #'load-theme)))

(after! magit
  (map!
   (:mode magit-status-mode
    (:leader
     (:prefix "g"
      :desc "Toggle magit todos" :nv "T" #'myme/toggle-magit-todos)))))

(after! forge
  (setq forge-alist (append '(("sqbu-github.cisco.com" "sqbu-github.cisco.com/api/v3" "sqbu-github.cisco.com" forge-github-repository)
                              ("wwwin-github.cisco.com" "wwwin-github.cisco.com/api/v3" "wwwin-github.cisco.com" forge-github-repository))
                            forge-alist)))

;; Email
(after! mu4e
  (map!
   (:mode mu4e-main-mode
     :desc "Update index" :nv "U" #'mu4e-update-index)
   (:mode mu4e-view-mode
    :desc "Fill long lines" :nv "M-Q" #'mu4e-view-fill-long-lines)
   (:mode mu4e-headers-mode
    :desc "Rerun search" :nv "gr" #'mu4e-headers-rerun-search))
  (setq mu4e-compose-mode-hook '(org-mu4e-compose-org-mode)
        mu4e-compose-format-flowed nil
        mu4e-maildir-shortcuts
        '((:maildir "/cisco/INBOX" :key ?i)
          (:maildir "/cisco/Archive" :key ?a)
          (:maildir "/cisco/Bugs" :key ?b)
          (:maildir "/cisco/Patches" :key ?p)
          (:maildir "/cisco/Review" :key ?r)
          (:maildir "/cisco/XAPI" :key ?x))
        mu4e-use-fancy-chars nil)
  (advice-add #'org~mu4e-mime-switch-headers-or-body
              :after
              (lambda ()
                (if (eq major-mode 'org-mode) (setq fill-column 66)))))

(after! org-mu4e
  (setq org-mu4e-convert-to-html nil))

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
     :desc "LSP execute code action" :nv "ca" #'lsp-execute-code-action)))
  (setq lsp-file-watch-ignored-directories (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\_build$"))
  (setq lsp-python-ms-executable (executable-find "python-language-server")))

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


;; Email
(set-email-account!
 "cisco"
  '((mu4e-sent-folder        . "/cisco/Sent")
    (mu4e-drafts-folder      . "/cisco/Drafts")
    (mu4e-trash-folder       . "/cisco/Trash")
    (mu4e-refile-folder      . "/cisco/Inbox")
    (smtpmail-smtp-server    . "outbound.cisco.com")
    (smtpmail-smtp-user      . "mmyrseth@cisco.com")
    (user-mail-address       . "mmyrseth@cisco.com")
    (mu4e-compose-signature  . "Martin"))
  t)


;; mu4e
(when (not (featurep 'org-mu4e))
  (add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp/mu4e"))


;; Safe local variables
(setq safe-local-variable-values
      '((eval setq-local org-roam-db-location
              (expand-file-name "org-roam.db" org-roam-directory))
        (eval setq-local org-roam-directory
              (expand-file-name
               (concat (locate-dominating-file default-directory ".dir-locals.el")
                       "roam")))
        (eval setq-local org-roam-dailies-directory
              (expand-file-name "dailies" org-roam-directory))))


(after! mu4e
  (require 'org-mu4e))

;; PureScript
(setq psc-ide-use-npm-bin t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs on WSL open links in Windows web browser
;; https://adam.kruszewski.name/2017/09/emacs-in-wsl-and-opening-links/
;; https://gist.github.com/minorugh/1770a6aa93df5fe55f70b4d72091ff76
(when (getenv "WSLENV")
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic
            search-web-default-browser 'browse-url-generic))))


;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))
