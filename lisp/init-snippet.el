
;; snippet engine
(straight-use-package 'yasnippet)

;; avoid error when using yasnippet with org-mode, ref:
;; https://www.reddit.com/r/emacs/comments/nj08dz/issues_with_yasnippet_in_emacs_272_lisp_error/
(setq yas-indent-line 'fixed)

;; Improve Emacs load speed by only using yasnippet in certain modes,
;; ref: https://github.com/joaotavora/yasnippet/issues/904.
(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'text-mode-hoook 'yas-minor-mode)

(straight-use-package 'yasnippet-snippets)

;; (define-key yas-minor-mode-map (kbd "M-z") 'yas-expand)
;; (define-key yas-minor-mode-map (kbd "<tab>") 'yas-expand)

;; change the shortcut to jump between tab stop poistions.
;; (define-key yas-keymap (kbd "M-j") 'yas-next-field-or-maybe-expand)
;; (define-key yas-keymap (kbd "M-k") 'yas-prev-field)

(provide 'init-snippet)
