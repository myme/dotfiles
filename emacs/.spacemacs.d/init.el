;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; Languages
     c-c++
     emacs-lisp
     (haskell :variables
              haskell-completion-backend 'intero)
     html
     markdown
     purescript
     python
     vimscript
     yaml
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)

     ;; Custom
     myme-javascript

     ;; Div
     helm
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t)

     ;; Mail + News
     gnus

     ;; Utils
     git
     (org :variables
          org-enable-reveal-js-support t)
     spell-checking
     syntax-checking
     themes-megapack
     version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      keychain-environment
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5
   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the lastest
   ;; version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil
   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         material
                         material-light
                         leuven
                         ;; spacemacs-dark
                         ;; spacemacs-light
                         )
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("DejaVu Sans Mono"
                               :size 12
                               :weight normal
                               :width normal
                               :powerline-scale 1.5)
   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy nil
   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil
   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  ;; Ag
  (defun my/helm-ag-find-project-file ()
    "Use ag to find a file relative to project root"
    (interactive)
    (let ((helm-ag-command-option "-g"))
      (helm-do-ag-project-root)))

  (defun my/helm-ag-find-file ()
    "Use ag to find a file relative to current directory"
    (interactive)
    (let ((helm-ag-command-option "-g"))
      (helm-do-ag)))

  (spacemacs/set-leader-keys "C-p" #'my/helm-ag-find-project-file)
  (spacemacs/set-leader-keys "M-p" #'my/helm-ag-find-file)

  ;; Buffers
  (spacemacs/set-leader-keys "b," #'rename-buffer)

  ;; Diffing
  (spacemacs/set-leader-keys "db" #'ediff-buffers)

  ;; Date
  (defun my/date ()
    (interactive)
    (insert (format-time-string "%Y-%m-%d")))

  (spacemacs/set-leader-keys "id" #'my/date)

  ;; Evil escape
  (setq-default evil-escape-key-sequence "qq")

  ;; C++
  (setq my-cc-style
        '("cc-mode"
          (c-offsets-alist . (
                              (innamespace . [0])
                              (inline-open . 0)))))
  (c-add-style "my-cc-style" my-cc-style)

  ;; Keychain
  (keychain-refresh-environment)

  ;; VCS/Git/Magit
  (spacemacs/set-leader-keys "gB" #'vc-annotate)
  (spacemacs/set-leader-keys "gff" #'magit-find-file)

  ;; Projectile
  (defvar my/projectile-prev-root "/tmp")

  (defun my/projectile-set-project-root ()
    (interactive)
    (setq my/projectile-prev-root (read-directory-name "Search at: " default-directory)))

  (defun my/projectile-read-project-root (orig-fun &rest args)
    (let ((res (ignore-errors (apply orig-fun nil))))
      (if res
          (progn
            (setq my/projectile-prev-root res)
            res)
        my/projectile-prev-root))
    )

  (advice-add 'projectile-project-root :around #'my/projectile-read-project-root)

  ;; Powerline
  (setq powerline-default-separator 'bar)

  ;; Purescript
  (define-key purescript-mode-map (kbd "M-/") #'company-complete)

  ;; Python
  (pyvenv-mode 1)

  ;; Org mode leader bindings
  (defun my/org-agenda-default-list (&optional arg)
    (interactive "P")
    (org-agenda arg "d"))

  ;; Org mode
  (defun my/org-clock-in-recent ()
    "Select a recently clocked task to clock into"
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'org-clock-in)))

  (defun my/org-is-project ()
    "Check if current task is a project (has subtasks)"
    (save-restriction
      (widen)
      (org-save-outline-visibility nil
        (org-reveal)
        (org-show-subtree)
        (let ((has-subtask)
              (subtree-end (save-excursion (org-end-of-subtree))))
          (save-excursion
            (while (and (outline-next-heading)
                        (< (point) subtree-end)
                        (not has-subtask))
              (if (member (org-get-todo-state) org-todo-keywords-1)
                  (setq has-subtask t)
                nil)))
          has-subtask))))

  (defun my/org-skip-non-stuck-projects ()
    "Skip projects which are not stuck (has NEXT task)"
    (save-restriction
      (widen)
      (org-save-outline-visibility nil
        (org-reveal)
        (org-show-subtree)
        (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
          (if (my/org-is-project)
              (let ((subtree-end (save-excursion (org-end-of-subtree))))
                (save-excursion
                  (forward-line 1)
                  (if (re-search-forward "^\*+ NEXT " subtree-end t)
                      next-headline
                    nil)))
            next-headline)))))

  (defun my/org-skip-non-archive-tasks ()
    "Skip tasks not ready for archiving"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
            (subtree-end (save-excursion (org-end-of-subtree t))))
        (if (member (org-get-todo-state) org-todo-keywords-1)
            (if (member (org-get-todo-state) org-done-keywords)
                (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                       (a-month-ago (* 60 60 24 (+ daynr 1)))
                       (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                       (this-month (format-time-string "%Y-%m-" (current-time)))
                       (subtree-is-current (save-excursion
                                             (forward-line 1)
                                             (and (< (point) subtree-end)
                                                  (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                  (if subtree-is-current
                      subtree-end ; Has a date in this month or last month, skip it
                    nil))  ; available to archive
              (or subtree-end (point-max)))
          next-headline))))

  (spacemacs/set-leader-keys "ao'" #'org-cycle-agenda-files)
  (spacemacs/set-leader-keys "aob" #'org-switchb)
  (spacemacs/set-leader-keys "aod" #'my/org-agenda-default-list)
  (spacemacs/set-leader-keys "aoi" #'my/org-clock-in-recent)
  (spacemacs/set-leader-keys "aoI" #'org-clock-in-last)
  (spacemacs/set-leader-keys "aoj" #'org-clock-goto)
  (spacemacs/set-leader-keys "aoP" #'org-pomodoro)
  )

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  ;; Do not write anything past this comment. This is where Emacs will
  ;; auto-generate custom variable definitions.
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(c-default-style
     (quote
      ((c-mode . "my-cc-style")
       (c++-mode . "my-cc-style")
       (java-mode . "java")
       (awk-mode . "awk")
       (other . "gnu"))))
   '(evil-want-Y-yank-to-eol nil)
   '(js-indent-level 2)
   '(js-switch-indent-offset 2)
   '(js2-strict-trailing-comma-warning nil)
   '(org-agenda-compact-blocks t)
   '(org-agenda-custom-commands
     (quote
      (("d" "Default agenda list"
        ((agenda ""
                 ((org-agenda-span 1)))
         (tags "REFILE"
               ((org-agenda-overriding-header "Tasks to Refile")))
         (tags-todo "-REFILE/NEXT"
                    ((org-agenda-overriding-header "Next items")))
         (tags-todo "-REFILE-CANCELED"
                    ((org-agenda-overriding-header "Stuck projects")
                     (org-agenda-skip-function
                      (function my/org-skip-non-stuck-projects))))
         (todo "HOLD|WAITING"
               ((org-agenda-overriding-header "Pending items")))
         (tags "-REFILE"
               ((org-agenda-overriding-header "Items to archive")
                (org-agenda-skip-function
                 (function my/org-skip-non-archive-tasks))
                (org-tags-match-list-sublevels nil))))
        nil nil))))
   '(org-agenda-files (quote ("~/Dropbox/org")))
   '(org-capture-templates
     (quote
      (("n" "A generic note" entry
        (file "refile.org")
        "* %? :NOTE:
%U
%a" :clock-in t :clock-resume t)
       ("t" "A TODO item" entry
        (file "refile.org")
        "* TODO %?
%U
%a" :clock-in t :clock-resume t)
       ("j" "Journal entry" entry
        (file+olp+datetree "work.org" "Journal")
        "* %?
%U" :clock-in t :clock-resume t))))
   '(org-clock-out-remove-zero-time-clocks t)
   '(org-clock-rounding-minutes 0)
   '(org-default-notes-file "~/Dropbox/org/notes.org")
   '(org-directory "~/Dropbox/org")
   '(org-outline-path-complete-in-steps nil)
   '(org-refile-targets
     (quote
      ((nil :maxlevel . 9)
       (org-agenda-files :maxlevel . 9))))
   '(org-refile-use-outline-path t)
   '(org-stuck-projects (quote ("" nil nil "")))
   '(org-time-stamp-rounding-minutes (quote (0 0)))
   '(org-todo-keywords
     (quote
      ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
       (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELED(c@/!)"))))
   '(org-todo-state-tags-triggers
     (quote
      (("CANCELLED"
        ("CANCELLED" . t))
       ("WAITING"
        ("WAITING" . t))
       ("HOLD"
        ("WAITING" . t)
        ("HOLD" . t))
       (done
        ("WAITING")
        ("HOLD"))
       ("TODO"
        ("WAITING")
        ("CANCELLED")
        ("HOLD"))
       ("NEXT"
        ("WAITING")
        ("CANCELLED")
        ("HOLD"))
       ("DONE"
        ("WAITING")
        ("CANCELLED")
        ("HOLD")))))
   '(package-selected-packages
     (quote
      (org-category-capture white-sand-theme symon string-inflection rebecca-theme realgud test-simple loc-changes load-relative password-generator org-brain impatient-mode helm-purpose window-purpose imenu-list exotica-theme evil-org evil-lion editorconfig dante cmake-ide levenshtein browse-at-remote prettier-js eslintd-fix vimrc-mode dactyl-mode winum solarized-theme madhat2r-theme fuzzy keychain-environment psci purescript-mode psc-ide helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-hoogle helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag flyspell-correct-helm ace-jump-helm-line yaml-mode mmm-mode markdown-toc markdown-mode gh-md rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode cython-mode company-anaconda anaconda-mode pythonic disaster company-c-headers cmake-mode clang-format web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode haml-mode emmet-mode company-web web-completion-data ox-reveal rjsx-mode intero hlint-refactor hindent haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern dash-functional tern coffee-mode zonokai-theme zenburn-theme zen-and-art-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pastels-on-dark-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme firebelly-theme farmhouse-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme xterm-color smeargle shell-pop orgit org-projectile org-present org org-pomodoro alert log4e gntp org-download multi-term magit-gitflow htmlize gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter flyspell-correct-ivy flyspell-correct flycheck-pos-tip pos-tip flycheck evil-magit magit magit-popup git-commit with-editor eshell-z eshell-prompt-extras esh-help diff-hl company-statistics company auto-yasnippet yasnippet auto-dictionary ac-ispell auto-complete ws-butler window-numbering which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline smex restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-hydra info+ indent-guide ido-vertical-mode hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-make helm helm-core google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump popup f s diminish define-word counsel-projectile projectile pkg-info epl counsel swiper ivy column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash async aggressive-indent adaptive-wrap ace-window ace-link avy quelpa package-build spacemacs-theme)))
   '(safe-local-variable-values
     (quote
      ((pyvenv-workon . "vega")
       (projectile-enable-caching . t))))
   '(split-height-threshold nil))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )
  )
