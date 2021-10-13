
;; lsp related settings
(straight-use-package 'lsp-mode)

;; it seems that lsp-mode supports auto-completion out of the box, no need
;; to use company-lsp, ref: https://github.com/tigersoldier/company-lsp/issues/146.

;; pylsp is the new maintained Python language server, it should work out of the box with lsp-mode.
;; See also https://github.com/emacs-lsp/lsp-mode/issues/2777.

;; Disable pylsp, use pyright instead, ref:
;; https://www.reddit.com/r/emacs/comments/hdrdmf/change_priority_of_lspmodes_language_servers/

;; (setq lsp-disabled-clients '(pylsp))

;; See https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
;; on how to disable various ui components.
;; show function signature
(setq lsp-signature-auto-activate nil)

;; whether show symbol highlighting
(setq lsp-enable-symbol-highlighting nil)

;; but do not show documentation
(setq lsp-signature-render-documentation nil)

;; show current function scope in header line
(setq lsp-headerline-breadcrumb-enable t)

(straight-use-package 'lsp-ui)

(setq lsp-ui-doc-enable nil)

(setq lsp-ui-doc-max-height 30) ;; doc window height

(setq lsp-ui-doc-position 'at-position) ;; seems does not work?

(setq lsp-modeline-code-actions-enable t)

;; some mappings
(define-key evil-normal-state-map (kbd "K") 'lsp-ui-doc-glance)
(define-key evil-normal-state-map (kbd "SPC r n") 'lsp-rename)
(define-key evil-normal-state-map (kbd "g d") 'lsp-find-definition)

;; (straight-use-package 'lsp-pyrerror-bitmapight)

(defun use-pyright ()
     (interactive)
     ;; (require 'lsp-pyright)
     (lsp-deferred)
     )

(add-hook 'python-mode-hook 'use-pyright)

(provide 'init-lsp)
