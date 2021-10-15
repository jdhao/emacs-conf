
;; show git gutter on left side
(straight-use-package 'git-gutter)

;; git-gutter settings
(when is-mac
    ;; the following does not work for Windows since there is no diff executable
    (custom-set-variables
    '(git-gutter:update-interval 1)))

(setq git-gutter:hide-gutter t)
(setq git-gutter:modified-sign "~")

;; save load time
(add-hook 'prog-mode-hook 'git-gutter-mode)

;; mappings for git-gutter
(define-key evil-normal-state-map (kbd "] c") 'git-gutter:next-hunk)
(define-key evil-normal-state-map (kbd "[ c") 'git-gutter:previous-hunk)

(provide 'init-gitgutter)
