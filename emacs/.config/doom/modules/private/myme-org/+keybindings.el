;;; private/myme-org/+keybindings.el -*- lexical-binding: t; -*-

(map!
 (:prefix "C-c"
      :desc "Org store link" :nv "l" #'org-store-link)
 (:leader
  (:prefix "o"
    (:desc "Org Agenda" :prefix "o"
      :desc "Org Agenda" :nv "a" #'org-agenda
      :desc "Org Clock In" :nv "i" #'myme/org-clock-in-recent
      :desc "Org Clock Out" :nv "o" #'org-clock-out
      ))))
