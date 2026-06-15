;;; ~/dotfiles/emacs/.config/doom/config.el -*- lexical-binding: t; -*-

;; Inherit PATH (and friends) from a login shell on macOS, where
;; launchd-started Emacs (daemon, Spotlight-launched .app) otherwise
;; only sees /usr/bin:/bin:/usr/sbin:/sbin.
(use-package! exec-path-from-shell
  :when (eq system-type 'darwin)
  :config
  (exec-path-from-shell-initialize))

;; macOS: when the daemon creates a frame for a remote emacsclient,
;; the new window draws but key events still go to whatever app was
;; frontmost. This only works because the launchd agent launches the
;; daemon from inside Emacs.app (see darwin.nix) — without that
;; bundle identity, `tell application "Emacs"` would fail to resolve.
(when (eq system-type 'darwin)
  (add-hook 'server-after-make-frame-hook
            (lambda ()
              (when (display-graphic-p)
                (ns-do-applescript "tell application \"Emacs\" to activate")))))

;; macOS /bin/ls rejects --dired, which Emacs passes from `insert-directory`,
;; making dired / find-file completion fail outside the current project with
;; "Listing directory failed but 'access-file' worked". Use coreutils' gls.
(when (and (eq system-type 'darwin) (executable-find "gls"))
  (setq insert-directory-program "gls"
        dired-listing-switches "-aBhl --group-directories-first"))

;; Doom

(set-popup-rules!
  '(("^\\*"  :slot 1 :vslot -1 :select t)
    ("^ \\*" :slot 1 :vslot -1 :size +popup-shrink-to-fit)
    ("^\\*Async" :slot 1 :vslot -1 :ttl nil)
    ("^\\*Compil\\(?:ation\\|e-Log\\)" :size 0.3 :ttl nil)
    ("^\\*Man " :side right :size 0.4 :vslot -6 :ttl 0 :quit t :select t)))

;; Transparency
(set-frame-parameter nil 'alpha-background @backgroundOpacity@)
(add-to-list 'default-frame-alist '(alpha-background . @backgroundOpacity@))


(defun myme/doom-ascii-banner ()
  "Return myme's custom ASCII banner for the Doom dashboard.
As of the `:ui dashboard' module (renamed from `:ui doom-dashboard' in
2.1), `+dashboard-ascii-banner-fn' must *return* the banner as a single
propertized string; the `+dashboard-widget-banner' widget handles the
centering, padding and insertion itself."
  (propertize
   (string-join
    '("                 ...',;;;;;;,''.."
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
      "                    E M A C S")
    "\n")
   'face '+dashboard-banner))


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

 ;; Banner (vars renamed +doom-dashboard-* -> +dashboard-* with the 2.1 module rename)
 +dashboard-banner-dir (concat doom-private-dir "banners/")
 +dashboard-banner-file "rocket-ship.svg"
 +dashboard-ascii-banner-fn #'myme/doom-ascii-banner

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

;; Pytest
(after! python-pytest
  (put 'python-pytest-executable 'safe-local-variable
       (lambda (s) (member s '("pytest" "poetry run pytest")))))

;; Projectile
(after! projectile
  (put 'lsp-disabled-clients 'safe-local-variable 'listp)
  (put 'projectile-project-compilation-dir 'safe-local-variable 'stringp)
  (put 'projectile-project-test-prefix 'safe-local-variable 'stringp))

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

;; LLM integration
(after! gptel
  (setq!
   gptel-model 'claude-sonnet-4-20250514
   gptel-backend (gptel-make-anthropic
                     "Claude"
                   :stream t
                   :key (lambda () (auth-source-pick-first-password :host "api.anthropic.com")))
   gptel-api-key (lambda () (auth-source-pick-first-password :host "api.openai.com"))))

;; Open Street Maps
(use-package! osm)

;; Formatting
;; See: https://github.com/doomemacs/doomemacs/issues/7438
(use-package! apheleia)

;; mu4e
(after! mu4e
  ;; FIXME: Broken in `doomemacs' due to `mu4e--view-gather-mime-parts' is removed in `mu4e'
  (map! :map mu4e-view-mode-map :desc "" :nv "A" #'mu4e-view-mime-part-action))

;; Additional configurations interpolated by nix
@doomConfigExtra@
