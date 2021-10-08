
;; snippet engine
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)

;; avoid error when using yasnippet with org-mode, ref:
;; https://www.reddit.com/r/emacs/comments/nj08dz/issues_with_yasnippet_in_emacs_272_lisp_error/
(setq yas-indent-line 'fixed)

(yas-global-mode 1)

;; (define-key yas-minor-mode-map (kbd "M-z") 'yas-expand)
;; (define-key yas-minor-mode-map (kbd "<tab>") 'yas-expand)

;; change the shortcut to jump between tab stop poistions.
;; (define-key yas-keymap (kbd "M-j") 'yas-next-field-or-maybe-expand)
;; (define-key yas-keymap (kbd "M-k") 'yas-prev-field)

(provide 'init-snippet)
