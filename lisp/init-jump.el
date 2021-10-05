
;; faster window jump
(straight-use-package 'ace-window)

;; key binding for ace-window
(global-set-key (kbd "M-o") 'ace-window)

;; faster buffer jump (like vim-sneak)
(straight-use-package 'avy)
;; load avy mode
(global-set-key (kbd "C-:") 'avy-goto-char-2)

(provide 'init-jump)
