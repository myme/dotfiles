;;; private/myme-org/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun myme/org-clock-in-recent ()
  "Select a recently clocked task to clock into"
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'org-clock-in)))

;;;###autoload
(defun myme/org-is-project ()
  "Check if current task is a project (has subtasks)"
  (save-restriction
    (widen)
    (let ((org-tags-exclude-from-inheritance nil))
      (member "PROJECT" (org-get-tags-at)))))

;;;###autoload
(defun myme/org-is-standalone ()
  "Check if current task is a project (has subtasks)"
  (save-restriction
    (widen)
    (org-save-outline-visibility nil
      (org-reveal)
      (save-excursion
        (if (not (myme/org-is-project))
            (let ((is-subtask))
              (while (and (not is-subtask)
                          (org-up-heading-safe))
                (if (member (org-get-todo-state) org-todo-keywords-1)
                    (setq is-subtask t)))
              (not is-subtask))
          nil)
        ))))

;;;###autoload
(defun myme/org-skip-non-standalone ()
  "Skip tasks which are not standalone"
  (save-restriction
    (widen)
    (org-save-outline-visibility nil
      (org-reveal)
      (org-show-subtree)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (myme/org-is-standalone)
            nil
          next-headline)))))

;;;###autoload
(defun myme/org-skip-non-archive-tasks ()
  "Skip tasks not ready for archiving.

Notes ready for archiving are todo items that are moved to a done
state and marked as closed more than 60 days ago.

This function jumps to the next sibling headline if the current
headline is a todo item that is not ready for archive. If it's
not a todo item it descends into the headline's subtree looking
for nested todo items."
  (save-restriction
    (widen)
    (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (next-sibling (save-excursion (or (outline-get-next-sibling) (point))))
           (state (org-get-todo-state)))
      (if (member state org-todo-keywords-1)
          (if (and (member state org-done-keywords)
                   (let ((days-ago 60)
                         (closed (org-entry-get nil "CLOSED")))
                     (and closed (> (- days-ago) (org-time-stamp-to-now closed)))))
              nil
            next-sibling)
        next-headline)
      )))

;;;###autoload
(defun myme/git-auto-sync ()
  (cl-letf (((symbol-function 'magit-anything-staged-p) (lambda () nil)))
    ;; Avoid asking for "stage all (y/n)"
    (magit-stage-modified))
  (when (magit-anything-staged-p)
    (let ((message (list "-m" (concat "Auto-commit: " (format-time-string "%Y-%m-%d %H:%M:%S")))))
      (message "Staged %s" (magit-anything-staged-p))
      (magit-commit-create message)))
  (magit-pull-from-pushremote ())
  (magit-push-current-to-pushremote ()))

;;;###autoload
(defun myme/org-auto-sync ()
  (interactive)
  (org-save-all-org-buffers)
  (dolist (org-dir (directory-files org-directory t (rx bos (not "."))))
    (when (f-directory-p org-dir)
      (let ((default-directory org-dir))
        (message "Syncing \"%s\"" (file-relative-name org-dir org-directory))
        (myme/git-auto-sync)))))


;;;###autoload
(defun myme/org-element-cache-reset-all ()
  "Refresh the org element cache for all org buffers."
  (interactive)
  (dolist (buffer (org-buffer-list))
    (with-current-buffer buffer
      (message "Refreshing \"%s\"" (buffer-name buffer))
      (org-element-cache-reset)))
  (message "Done!"))

;;;###autoload
(defun myme/org-visit-all ()
  "Visit all org-mode agenda files"
  (interactive)
  (save-excursion
    (dolist (file (org-agenda-files))
      (message "Visiting \"%s\"" file)
      (find-file-noselect file)))
  (message "Done!"))

;;;###autoload
(defun myme/org-select-project ()
  "Select org-mode related variables based on a projectile project"
  (interactive)
  (let* ((projectile-current-project-on-switch 'keep)
         (projects (projectile-relevant-known-projects)))
    (if projects
        (projectile-completing-read
         "Select org project: " projects
         :action #'myme/org-set-project-dir)
      (user-error "There are no known projects"))))

;;;###autoload
(defun myme/org-roam-ui-open ()
  "Launch org-roam-ui without opening multiple browser tabs"
  (interactive)
  (let ((org-roam-ui-open-on-start nil))
    (org-roam-ui-open)))

;;;###autoload
(defun myme/org-set-project-dir (project)
  "Set org-mode related variables based on project dir"
  (setq
   org-directory (expand-file-name project)
   org-id-locations-file (expand-file-name ".org-ids" project)
   org-attach-id-dir (expand-file-name ".attach" project)
   org-roam-directory (expand-file-name "roam" project)
   org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory)
   org-roam-dailies-directory (expand-file-name "dailies" org-roam-directory))
  (myme/org-update-agenda-files nil)
  (message "Set org project to %s" project))

;;;###autoload
(defun myme/org-update-agenda-files (days-backwards)
  "Update org-agenda-files based on org-roam-dailies-directory"
  (interactive "P")
  (let ((default-days 60))
    (setq
     org-agenda-files
     (append (list org-directory) (myme/org-last-dailies (or days-backwards default-days)))))
  (message "Updated org-agenda-files to %s" org-agenda-files))

;;;###autoload
(defun myme/org-last-dailies (days-backwards &optional ignore-future)
  "Expand to <count> last Org-Roam daily files.

DAYS-BACKWARDS: Number of days to look back. The special value -1 expands to all
                files.
IGNORE-FUTURE: When non-nil, exclude files with dates in the future."
  (let* ((current-time (current-time))
         (cutoff-time (time-subtract current-time (days-to-time days-backwards)))
         (daily-regexp  "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org\\'")
         (all-files (directory-files org-roam-dailies-directory t daily-regexp)))
    (seq-filter
     (lambda (file)
       (let* ((filename (file-name-nondirectory file))
              (date-string (substring filename 0 10))
              (file-time (date-to-time date-string)))
         (and (or (= days-backwards -1)
                  (time-less-p cutoff-time file-time))
              (or (not ignore-future)
                  (time-less-p file-time current-time)))))
     all-files)))

;;;###autoload
(defun myme/org-paste-html-clipboard ()
  "Paste HTML from clipboard into org-mode buffer (https://emacs.stackexchange.com/a/12124)"
  (interactive)
  (kill-new (shell-command-to-string "xclip-to-org"))
  (yank))

;;;###autoload
(defun myme/org-read-element-body ()
  "Return the body of the org element at point.

Exclude header and property drawers."
  (interactive)
  (save-excursion
    ;; If in agenda, go to the actual org buffer
    (when (derived-mode-p 'org-agenda-mode)
      (org-agenda-switch-to))

    (org-back-to-heading-or-point-min t)
    (let ((element (org-element-at-point)))
      (when (eq (org-element-type element) 'headline)
        (let* ((contents-begin (org-element-property :contents-begin element))
               (contents-end (org-element-property :contents-end element)))
          (when (and contents-begin contents-end)
            (goto-char contents-begin)
            (org-end-of-meta-data t)
            (let ((body-start (point)))
              (when (< body-start contents-end)
                (buffer-substring-no-properties body-start contents-end)))))))))

;;;###autoload
(defun myme/org-copy-and-capture-to-daily ()
  "Copy current heading body to daily note with link and clock in."
  (interactive)
  (let ((link nil)
        (body nil))

    ;; Store link and copy body
    (save-excursion
      ;; If in agenda, go to the actual org buffer
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-switch-to))

      (org-back-to-heading-or-point-min t)
      (setq link (org-store-link nil))
      (setq body (myme/org-read-element-body)))

    ;; Capture to daily note
    (org-roam-dailies-capture-today nil "d")

    ;; Insert link and body, then clock in
    (when link
      (insert (format "%s\n\n" link)))
    (when body
      (insert body))
    (org-clock-in)))
