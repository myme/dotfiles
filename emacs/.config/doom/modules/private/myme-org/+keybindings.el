;;; private/myme-org/+keybindings.el -*- lexical-binding: t; -*-

(map!
 (:leader
  (:prefix "o"
    (:desc "Org Agenda" :prefix "o"
      :desc "Org Agenda" :nv "a" #'org-agenda
      :desc "Org Default Agenda" :nv "d" #'myme/org-agenda-default-list
      :desc "Org Clock In" :nv "i" #'myme/org-clock-in-recent
      :desc "Org Clock Out" :nv "o" #'org-clock-out
      )))
 (:after org
   (:map org-read-date-minibuffer-local-map
     ;; Calendar navigation
     "M-h" (lambda! (org-eval-in-calendar '(calendar-backward-day 1)))
     "M-l" (lambda! (org-eval-in-calendar '(calendar-forward-day 1)))
     "M-k" (lambda! (org-eval-in-calendar '(calendar-backward-week 1)))
     "M-j" (lambda! (org-eval-in-calendar '(calendar-forward-week 1)))
     "M-H" (lambda! (org-eval-in-calendar '(calendar-backward-month 1)))
     "M-L" (lambda! (org-eval-in-calendar '(calendar-forward-month 1)))
     "M-K" (lambda! (org-eval-in-calendar '(calendar-backward-year 1)))
     "M-J" (lambda! (org-eval-in-calendar '(calendar-forward-year 1)))
     )))

(map! :prefix "C-c"
      :desc "Org store link" :nv "l" #'org-store-link)

