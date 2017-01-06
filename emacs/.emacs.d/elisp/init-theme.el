;;; package --- Here be themes
;;; Commentary:
;;; Code:

(use-package darkokai-theme
  :ensure t
  :defer t
  :init
  (setq darkokai-mode-line-padding 1)
  (load-theme 'darkokai t))


(use-package powerline
  :ensure t
  :config

  (use-package powerline-evil
    :ensure t)

  (setq powerline-default-separator
    (if (display-graphic-p) 'arrow nil))

  (powerline-evil-vim-color-theme))


(provide 'init-theme)
;;; init-theme.el ends here
