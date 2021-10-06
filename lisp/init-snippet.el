
;; snippet engine
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)

(yas-global-mode 1)

;; (define-key yas-minor-mode-map (kbd "M-z") 'yas-expand)
;; (define-key yas-minor-mode-map (kbd "<tab>") 'yas-expand)

;; change the shortcut to jump between tab stop poistions.
;; (define-key yas-keymap (kbd "M-j") 'yas-next-field-or-maybe-expand)
;; (define-key yas-keymap (kbd "M-k") 'yas-prev-field)

(provide 'init-snippet)
