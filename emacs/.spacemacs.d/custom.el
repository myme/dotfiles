;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style
   (quote
    ((c++-mode . "stroustrup")
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
             ((org-agenda-overriding-header "Tasks to Refile")
              (org-tags-match-list-sublevels nil)))
       (todo "NEXT"
             ((org-agenda-overriding-header "Next items")))
       (todo "TODO"
             ((org-agenda-overriding-header "TODOs"))))
      nil nil))))
 '(org-agenda-files (quote ("~/Dropbox/org")))
 '(org-capture-templates
   (quote
    (("n" "A generic note" entry
      (file "refile.org")
      (file "~/.spacemacs.d/note.template")
      :clock-in t :clock-resume t)
     ("t" "A TODO item" entry
      (file "refile.org")
      (file "~/.spacemacs.d/todo.template")
      :clock-in t :clock-resume t)
     ("j" "Journal entry" entry
      (file+datetree "journal.org")
      (file "~/.spacemacs.d/journal.template")
      :clock-in t :clock-resume t))))
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
 '(org-time-stamp-rounding-minutes (quote (0 0)))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "WAITING(w)" "|" "CANCELLED(c)"))))
 '(package-selected-packages
   (quote
    (mmm-mode markdown-toc markdown-mode gh-md rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode cython-mode company-anaconda anaconda-mode pythonic disaster company-c-headers cmake-mode clang-format web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode haml-mode emmet-mode company-web web-completion-data ox-reveal rjsx-mode intero hlint-refactor hindent haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern dash-functional tern coffee-mode zonokai-theme zenburn-theme zen-and-art-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pastels-on-dark-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme firebelly-theme farmhouse-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme xterm-color smeargle shell-pop orgit org-projectile org-present org org-pomodoro alert log4e gntp org-download multi-term magit-gitflow htmlize gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter flyspell-correct-ivy flyspell-correct flycheck-pos-tip pos-tip flycheck evil-magit magit magit-popup git-commit with-editor eshell-z eshell-prompt-extras esh-help diff-hl company-statistics company auto-yasnippet yasnippet auto-dictionary ac-ispell auto-complete ws-butler window-numbering which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline smex restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-hydra info+ indent-guide ido-vertical-mode hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-make helm helm-core google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump popup f s diminish define-word counsel-projectile projectile pkg-info epl counsel swiper ivy column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash async aggressive-indent adaptive-wrap ace-window ace-link avy quelpa package-build spacemacs-theme)))
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
