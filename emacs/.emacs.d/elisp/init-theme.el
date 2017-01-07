;;; package --- Here be themes
;;; Commentary:
;;; Code:

(use-package darkokai-theme
  :ensure t
  :defer t
  :init
  (setq darkokai-mode-line-padding 1)
  (load-theme 'darkokai t))


(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-spacemacs-theme)
  )


(provide 'init-theme)
;;; init-theme.el ends here
