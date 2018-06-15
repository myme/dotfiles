;;; private/myme-config/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun myme/avy-goto-url()
  "Use avy to go to an URL in the buffer."
  (interactive)
  (avy--generic-jump "https?://" nil 'pre))

;;;###autoload
(defun myme/avy-open-url ()
  "Use avy to select an URL in the buffer and open it."
  (interactive)
  (save-excursion
    (myme/avy-goto-url)
    (browse-url-at-point)))

