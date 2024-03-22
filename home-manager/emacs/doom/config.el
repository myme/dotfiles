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
 (:prefix
  "C-c i"
  (:desc "Insert today's date" "d" #'myme/insert-date)
  (:desc "Insert current timestamp" "t" #'myme/insert-timestamp))
 (:prefix
  "g z"
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
   :desc "Compile" :nv "k" #'kill-compilation
   :desc "Hints"   :nv "h" #'myme/lsp-inlay-hints-mode)
  (:prefix "d"
   :desc "Ediff buffers" :nv "b" #'ediff-buffers)
  (:prefix "o"
   (:prefix "M" :desc "Maps"
    :desc "Search maps" :nv "s" #'osm-search))
  (:prefix "p"
   :desc "Open dired in project" :nv "d" #'projectile-dired
   :desc "Run async cmd in project" :nv "&" #'projectile-run-async-shell-command-in-root
   :desc "Test project" :nv "t" #'projectile-test-project)
  (:prefix "q"
   :desc "Kill emacs" :nv "k" #'save-buffers-kill-emacs)
  (:prefix "s"
   :desc "Consult complex commands" :nv "c" #'consult-complex-command)
  (:prefix ("t" . "toggle")
   :desc "Fill Column Indicator"        "C" #'global-display-fill-column-indicator-mode
   :desc "Flymake"                      "c" #'flymake-mode
   (:when (modulep! :checkers syntax)
     :desc "Flycheck"                   "c" #'flycheck-mode)
   :desc "Toggle buffer auto-formatting" :nv "f" #'format-all-mode)
  (:desc "theme" :prefix "T"
   :desc "Switch theme" :nv "T" #'load-theme)))

;; Doom pop-ups
(set-popup-rule! "^\\*info\\*$" :slot 2 :vslot 2 :size 0.45 :select t :side 'right)
(+popup-cleanup-rules-h)

(after! magit
  (map!
   (:mode
    magit-status-mode
    (:leader
     (:prefix "g"
      :desc "Toggle magit todos" :nv "T" #'myme/toggle-magit-todos)))))

;; Links
(setq ace-link-fallback-function
      (lambda ()
        "Add ace-link functions for additional modes."
        (interactive)
        (cond ((eq major-mode 'Man-mode)
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
   (:mode
    lsp-mode
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
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (setq copilot-node-executable "@nodeExecutable@"))

;; ChatGPT integration
(use-package! chatgpt-shell
  :config
  (setq chatgpt-shell-openai-key
        (lambda () (auth-source-pick-first-password :host "api.openai.com"))))

;; Open Street Maps
(use-package! osm)

;; Formatting
;; See: https://github.com/doomemacs/doomemacs/issues/7438
(use-package! apheleia)

;; Additional configurations interpolated by nix
@doomConfigExtra@
