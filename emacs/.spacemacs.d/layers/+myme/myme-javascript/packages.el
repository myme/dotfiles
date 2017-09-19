(defconst myme-javascript-packages
  '(
    add-node-modules-path
    company-flow
    eslintd-fix
    flycheck
    prettier-js
    rjsx-mode))


(defun myme-javascript/init-eslintd-fix ()
  (use-package eslintd-fix
    :defer t
    :commands eslintd-fix-mode
    :init
    (progn
      (add-hook 'rjsx-mode-hook #'eslintd-fix-mode t))))

(defun myme-javascript/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

      (setq
       js2-mode-show-strict-warnings nil
       js2-mode-show-parse-errors nil
       js-indent-level 2
       js2-basic-offset 2
       js2-strict-trailing-comma-warning nil
       js2-strict-missing-semi-warning nil)

      (advice-add #'js-jsx-indent-line
                  :after
                  #'myme-javascript/js-jsx-indent-line-align-closing-bracket)
      (add-hook 'rjsx-mode-hook #'myme-javascript/use-eslint-from-node-modules))
    :config
    (progn
      (modify-syntax-entry ?_ "w" js2-mode-syntax-table)
      ;; Disable rjsx-mode tag insert smartness
      (define-key rjsx-mode-map "<" nil)
      (define-key rjsx-mode-map (kbd "C-d") nil))))

(defun myme-javascript/post-init-add-node-modules-path ()
  (with-eval-after-load 'rjsx-mode
    (add-hook 'rjsx-mode-hook #'add-node-modules-path)))

(defun myme-javascript/post-init-company-flow ()
  (spacemacs|add-company-backends
    :backends
    '((company-flow :with company-dabbrev-code)
      company-files)))

(defun myme-javascript/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (push 'javascript-jshint flycheck-disabled-checkers)
    (push 'json-jsonlint flycheck-disabled-checkers))
  (spacemacs/add-flycheck-hook 'rjsx-mode))

(defun myme-javascript/init-prettier-js ()
  (use-package prettier-js
    :defer t
    :init
    (progn
      (add-hook 'rjsx-mode-hook 'prettier-js-mode)
      (setq prettier-js-args '(
                               "--trailing-comma" "es5"
                               "--bracket-spacing" "false"
                               "--no-semi"
                               "--single-quote")))))
