(defconst myme-org-packages nil)

(spacemacs|use-package-add-hook org
  :post-config
  (progn
    (add-to-list 'org-modules 'org-habit)
    (setq
     org-directory "~/Dropbox/org"
     org-default-notes-file (expand-file-name "notes.org" "~/Dropbox/org/notes.org"))))
