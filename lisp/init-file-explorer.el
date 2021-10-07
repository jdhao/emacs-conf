
;;; settins for dired
(setq dired-listing-switches "-al -h") ;; options passed to ls

(straight-use-package 'dired-k)
(setq dired-k-style 'git)

;; always execute dired-k when dired buffer is opened
(add-hook 'dired-initial-position-hook 'dired-k)

(straight-use-package 'all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(straight-use-package 'treemacs)
(global-set-key (kbd "C-c s") 'treemacs)

(provide 'init-file-explorer)
