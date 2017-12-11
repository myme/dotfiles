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
    (org-save-outline-visibility nil
      (org-reveal)
      (org-show-subtree)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree))))
        (save-excursion
          (while (and (outline-next-heading)
                      (< (point) subtree-end)
                      (not has-subtask))
            (if (member (org-get-todo-state) org-todo-keywords-1)
                (setq has-subtask t)
              nil)))
        has-subtask))))

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

(defun my/org-skip-project ()
  "Skip projects"
  (save-restriction
    (widen)
    (org-save-outline-visibility nil
      (org-reveal)
      (org-show-subtree)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (my/org-is-project)
            next-headline
          nil)))))

(defun my/org-skip-non-stuck-project ()
  "Skip projects which are not stuck (has NEXT task)"
  (save-restriction
    (widen)
    (org-save-outline-visibility nil
      (org-reveal)
      (org-show-subtree)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (my/org-is-project)
            (let ((subtree-end (save-excursion (org-end-of-subtree))))
              (save-excursion
                (forward-line 1)
                (if (re-search-forward "^\*+ NEXT " subtree-end t)
                    next-headline
                  nil)))
          next-headline)))))

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
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))
