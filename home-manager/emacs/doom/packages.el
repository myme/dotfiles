;; -*- no-byte-compile: t; -*-
;;; ~/dotfiles/emacs/.config/doom/packages.el

;; Misc
(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
(package! keychain-environment)

;; JavaScript
(package! prettier-js)

;; Python
(package! pyvenv)
