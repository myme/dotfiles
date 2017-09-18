;; To use eslint daemon (eslint_d)
;; (defun myme-javascript/eslintd-set-flycheck-executable ()
;;   (interactive)
;;   (when-let (eslintd-executable (executable-find "eslint_d"))
;;     (make-variable-buffer-local 'flycheck-javascript-eslint-executable)
;;     (setq flycheck-javascript-eslint-executable eslintd-executable)))

;; Inspired by http://blog.binchen.org/posts/indent-jsx-in-emacs.html
(defun myme-javascript/js-jsx-indent-line-align-closing-bracket ()
  "Workaround sgml-mode and align closing bracket with opening bracket"
  (save-excursion
    (beginning-of-line)
    (when (looking-at-p "^ +\/?> *$")
      (delete-char sgml-basic-offset))))

;; Find eslint binary relative to current file
(defun myme-javascript/use-eslint-from-node-modules ()
  (let ((root (locate-dominating-file
               (or (buffer-file-name) default-directory)
               (lambda (dir)
                 (let ((eslint (expand-file-name "node_modules/.bin/eslint" dir)))
                   (and eslint (file-executable-p eslint)))))))
    (when root
      (let ((eslint (expand-file-name "node_modules/.bin/eslint" root)))
        (setq-local flycheck-javascript-eslint-executable eslint)))))
