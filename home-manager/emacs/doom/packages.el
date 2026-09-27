;; -*- no-byte-compile: t; -*-
;;; ~/dotfiles/emacs/.config/doom/packages.el

;; LLMs
;; (package! chatgpt-shell
;;   :recipe (:host github :repo "xenodium/chatgpt-shell"))

;; Misc
(package! keychain-environment)
(when (eq system-type 'darwin)
  (package! exec-path-from-shell))

;; JavaScript
(package! prettier-js)

;; Open Street Maps
(package! osm)

;; Python
(package! pyvenv)
