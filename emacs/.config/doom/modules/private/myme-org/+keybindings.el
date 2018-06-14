;;; private/myme-org/+keybindings.el -*- lexical-binding: t; -*-

(map! :leader
      (:prefix "o"
        (:desc "Org Agenda" :prefix "a"
            :desc "Org Agenda" :nv "a" #'org-agenda
            :desc "Org Default Agenda" :nv "d" #'myme/org-agenda-default-list
          )))

