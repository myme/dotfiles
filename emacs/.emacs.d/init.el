;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:


;; Will be added back if removed
(package-initialize)

(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(require 'init)
(require 'init-theme)
(require 'init-extensions)


(require 'lang-python)
(require 'lang-javascript)
(require 'lang-haskell)
(require 'lang-c)
