(defconst myme-org-packages nil)

(spacemacs|use-package-add-hook org
  :post-config
  (setq
   org-directory "~/Dropbox/org"
   org-default-notes-file (expand-file-name "notes.org" "~/Dropbox/org/notes.org")))
