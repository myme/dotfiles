;;; private/myme-org/+keybindings.el -*- lexical-binding: t; -*-

(map! :leader
      (:prefix "o"
        :desc "Org Default Agenda" :nv "a" #'myme/org-agenda-default-list))

