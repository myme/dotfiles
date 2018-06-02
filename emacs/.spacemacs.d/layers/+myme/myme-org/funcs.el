;; Org mode leader bindings
(defun my/org-agenda-default-list (&optional arg)
  "Default Org Agenda view"
  (interactive "P")
  (org-agenda arg "d"))

;; Org mode
(defun my/org-clock-in-recent ()
  "Select a recently clocked task to clock into"
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'org-clock-in)))

(defun my/org-is-project ()
  "Check if current task is a project (has subtasks)"
  (save-restriction
    (widen)
    (let ((org-tags-exclude-from-inheritance nil))
      (member "PROJECT" (org-get-tags-at)))))

(defun my/org-is-standalone ()
  "Check if current task is a project (has subtasks)"
  (save-restriction
    (widen)
    (org-save-outline-visibility nil
      (org-reveal)
      (save-excursion
        (if (not (my/org-is-project))
            (let ((is-subtask))
              (while (and (not is-subtask)
                          (org-up-heading-safe))
                (if (member (org-get-todo-state) org-todo-keywords-1)
                    (setq is-subtask t)))
              (not is-subtask))
          nil)
        ))))

(defun my/org-skip-non-standalone ()
  "Skip tasks which are not standalone"
  (save-restriction
    (widen)
    (org-save-outline-visibility nil
      (org-reveal)
      (org-show-subtree)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (my/org-is-standalone)
            nil
          next-headline)))))

(defun my/org-skip-non-archive-tasks ()
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
