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
  "Skip tasks not ready for archiving"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (and (member (org-get-todo-state) org-done-keywords)
               (let ((days-ago 60)
                     (closed (org-entry-get nil "CLOSED")))
                 (and closed (> (- days-ago) (org-time-stamp-to-now closed)))))
          nil
        next-headline))))

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
