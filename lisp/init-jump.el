
;; faster window jump
(straight-use-package 'ace-window)

;; key binding for ace-window
(global-set-key (kbd "M-o") 'ace-window)

;; faster buffer jump (like vim-sneak)
(straight-use-package 'avy)

(define-key evil-normal-state-map (kbd "f") 'avy-goto-char-2)

;; Emacs use ?a to represent a character, see https://stackoverflow.com/q/19862517/6064933.
(setq avy-keys (number-sequence ?a ?z))

(setq avy-style 'at-full)

(setq avy-background t) ;; obscure background when searching

;; 开启根据拼音首字母跳转
(straight-use-package 'ace-pinyin)
(ace-pinyin-global-mode +1)

(provide 'init-jump)
