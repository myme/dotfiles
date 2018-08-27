;;; private/myme-org/init.el -*- lexical-binding: t; -*-

;; Org-mode
(after! org
  (setq
   ;; org-enable-reveal-js-support t
   org-agenda-compact-blocks t
   org-agenda-custom-commands
   (quote
    (("r" "Tasks to refile"
      ((tags "REFILE"
             ((org-agenda-overriding-header "Tasks to refile"))))
      nil nil)
     ("p" "Pending items"
       ((todo "HOLD|WAITING"
             ((org-agenda-overriding-header "Pending items"))))
      nil nil)
     ("$" "Items to archive"
      ((tags "-REFILE"
             ((org-agenda-overriding-header "Items to archive")
              (org-agenda-skip-function
               (function myme/org-skip-non-archive-tasks))
              (org-tags-match-list-sublevels nil))))
      nil nil)
     ("n" "Next items"
      ((tags-todo "-REFILE-SCHEDULED>\"<now>\"/NEXT"
                  ((org-agenda-overriding-header "Next items"))))
      nil nil)
     ("o" "Standalone tasks"
      ((tags-todo "-REFILE-PROJECT-SCHEDULED>\"<now>\"/TODO"
                  ((org-agenda-overriding-header "Standalone tasks")
                   (org-agenda-skip-function
                    (function myme/org-skip-non-standalone)))))
      nil nil)
     ))
   org-agenda-files (quote ("~/Dropbox/org"))
   org-agenda-span 1
   org-agenda-start-day "Today"
   org-agenda-start-with-log-mode t
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
   org-clock-report-include-clocking-task t
   org-clock-rounding-minutes 0
   org-footnote-section nil
   org-log-done 'time
   org-log-into-drawer "LOGBOOK"
   org-outline-path-complete-in-steps nil
   org-refile-targets
   (quote
    ((nil :maxlevel . 9)
     (org-agenda-files :maxlevel . 9)))
   org-refile-use-outline-path t
   org-stuck-projects (quote ("PROJECT-SCHEDULED>\"<now>\"/-HOLD-WAITING" ("NEXT") nil ""))
   org-tags-exclude-from-inheritance '("PROJECT")
   org-time-stamp-rounding-minutes (quote (0 0))
   org-todo-keywords
   (quote
    ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
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
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-latex-classes
               '("article" "\\documentclass[11pt]{article}\n\\usepackage{parskip}\n\\setlength{\\parindent}{0}"
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(load! "+keybindings")

