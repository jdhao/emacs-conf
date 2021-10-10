;;; -*- lexical-binding: t -*-

;; custom config directory
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-basic)

(require 'init-pack-manager)

(require 'init-nolitter)

(require 'init-evil)

(require 'init-ui)

(require 'init-file-explorer)

(require 'init-fuzzy-finder)

(require 'init-autocomplete)

(require 'init-git)

(require 'init-gitgutter)

(require 'init-jump)

(require 'init-snippet)

(require 'init-md)

(require 'init-org)

(require 'init-cjk)

(require 'init-parens)

(require 'init-lint)

;; (require 'init-telega)

(require 'init-misc)

(require 'init-blackout)
