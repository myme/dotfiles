;;; ~/dotfiles/emacs/.config/doom/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun myme/avy-goto-url()
  "Use avy to go to an URL in the buffer."
  (interactive)
  (avy-jump "https?://"))

;;;###autoload
(defun myme/avy-open-url ()
  "Use avy to select an URL in the buffer and open it."
  (interactive)
  (save-excursion
    (myme/avy-goto-url)
    (browse-url-at-point)))

;;;###autoload
(defun myme/insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;;;###autoload
(defun myme/insert-timestamp ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

;;;###autoload
(defun myme/use-eslint-from-node-modules ()
  "Find eslint binary relative to current file."
  (let ((root (locate-dominating-file
               (or (buffer-file-name) default-directory)
               (lambda (dir)
                 (let ((eslint (expand-file-name "node_modules/.bin/eslint" dir)))
                   (and eslint (file-executable-p eslint)))))))
    (when root
      (let ((eslint (expand-file-name "node_modules/.bin/eslint" root)))
        (setq-local flycheck-javascript-eslint-executable eslint)))))

;;;###autoload
(defun myme/toggle-magit-todos (arg)
  (interactive "P")
  (if magit-todos-mode
      (magit-todos-mode -1)
    (magit-todos-mode arg))
  (magit-refresh))


;;;###autoload
(defun myme/lsp-inlay-hints-mode ()
  (interactive)
  (if (modulep! :tools lsp +eglot)
      (eglot-inlay-hints-mode)
    (lsp-inlay-hints-mode)))
