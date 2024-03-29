;;; private/myme-org/+keybindings.el -*- lexical-binding: t; -*-

(map!
 (:prefix "C-c"
  :desc "Org store link" :nv "l" #'org-store-link)
 (:leader
  (:prefix "o"
   (:prefix "o" :desc "Org Agenda"
    :desc "Org Agenda" :nv "a" #'org-agenda
    :desc "Org Clock In" :nv "i" #'myme/org-clock-in-recent
    :desc "Org Clock Out" :nv "o" #'org-clock-out
    :desc "Select Org-mode project" :nv "p" #'myme/org-select-project
    :desc "Update Org Agenda files" :nv "f" #'myme/org-update-agenda-files
    :desc "Visit all org-mode files" :nv "r" #'myme/org-element-cache-reset-all
    :desc "Open Org-Roam UI" :nv "u" #'myme/org-roam-ui-open
    :desc "Stop Org-Roam UI" :nv "U" (lambda () (interactive) (org-roam-ui-mode 0))
    :desc "Visit all org-mode files" :nv "v" #'myme/org-visit-all)
   (:prefix "s" :desc "Org Sync"
    (:desc "Org sync all" :nv "s" #'myme/org-auto-sync))
   )))
