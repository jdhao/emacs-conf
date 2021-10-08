;; customize how major and minor modes are displayed on modeline
(straight-use-package
 '(blackout :host github :repo "raxod502/blackout"))

;; do not show some mode on modeline using blackout
;; for minor modes
(blackout 'eldoc-mode)
(blackout 'gcmh-mode)
(blackout 'evil-commentary-mode)
(blackout 'git-gutter-mode)
(blackout 'anzu-mode)
(blackout 'ivy-posframe-mode)
(blackout 'ivy-mode)
(blackout 'counsel-mode)
(blackout 'smartparens-mode)
(blackout 'which-key-mode)
(blackout 'company-mode)
(blackout 'super-save-mode)
(blackout 'undo-tree-mode)
(blackout 'visual-line-mode)
(blackout 'whitespace-mode)
(blackout 'yas-minor-mode)
(blackout 'pangu-spacing-mode)
(blackout 'company-posframe-mode)

;; for major modes
(blackout 'emacs-lisp-mode "Elisp")
(blackout 'dashboard-mode)
(blackout 'lisp-interaction-mode)

(provide 'init-blackout)
