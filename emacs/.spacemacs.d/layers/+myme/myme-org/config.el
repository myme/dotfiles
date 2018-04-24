(setq
 org-enable-reveal-js-support t
 org-agenda-compact-blocks t
 org-agenda-custom-commands
 (quote
  (("d" "Default agenda list"
    ((agenda ""
             ((org-agenda-span 1)))
     (tags "REFILE"
           ((org-agenda-overriding-header "Tasks to Refile")))
     (tags-todo "-REFILE/NEXT"
                ((org-agenda-overriding-header "Next items")
                 (org-agenda-skip-function
                  (function my/org-skip-project))))
     (tags-todo "-REFILE-CANCELED"
                ((org-agenda-overriding-header "Stuck projects")
                 (org-agenda-skip-function
                  (function my/org-skip-non-stuck-project))))
     (todo "HOLD|WAITING"
           ((org-agenda-overriding-header "Pending items")))
     (tags-todo "-REFILE/TODO"
                ((org-agenda-overriding-header "Standalone tasks")
                 (org-agenda-skip-function
                  (function my/org-skip-non-standalone))))
     (tags "-REFILE"
           ((org-agenda-overriding-header "Items to archive")
            (org-agenda-skip-function
             (function my/org-skip-non-archive-tasks))
            (org-tags-match-list-sublevels nil))))
    ((org-agenda-start-with-log-mode t)) nil)))
 org-agenda-files (quote ("~/Dropbox/org"))
 org-capture-templates
 (quote
  (("n" "A generic note" entry
    (file "refile.org")
    "* %? :NOTE:
%U
%a")
   ("t" "A TODO item" entry
    (file "refile.org")
    "* TODO %?
%U
%a" :clock-in t :clock-resume t)
   ("j" "Journal entry" entry
    (file+olp+datetree "work.org" "Journal")
    "* %?
%U" :clock-in t :clock-resume t)))
 org-clock-out-remove-zero-time-clocks t
 org-clock-rounding-minutes 0
 org-footnote-section nil
 org-outline-path-complete-in-steps nil
 org-refile-targets
 (quote
  ((nil :maxlevel . 9)
   (org-agenda-files :maxlevel . 9)))
 org-refile-use-outline-path t
 org-stuck-projects (quote ("" nil nil ""))
 org-time-stamp-rounding-minutes (quote (0 0))
 org-todo-keywords
 (quote
  ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
   (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELED(c@/!)")))
 org-todo-state-tags-triggers
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
    ("HOLD"))))
 )
