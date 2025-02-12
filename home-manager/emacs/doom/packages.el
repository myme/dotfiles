;; -*- no-byte-compile: t; -*-
;;; ~/dotfiles/emacs/.config/doom/packages.el

;; LLMs
;; (package! chatgpt-shell
;;   :recipe (:host github :repo "xenodium/chatgpt-shell"))
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))
(package! gptel)

;; Misc
(package! keychain-environment)

;; JavaScript
(package! prettier-js)

;; Open Street Maps
(package! osm)

;; Python
(package! pyvenv)
