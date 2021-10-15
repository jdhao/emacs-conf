
;; 输入中文
(straight-use-package 'pyim)
(straight-use-package 'pyim-basedict)

;; 显示 5 个候选词。
(setq pyim-page-length 9)
(setq-default default-input-method "pyim")

(pyim-default-scheme 'quanpin)
(global-set-key (kbd "C-\\") 'toggle-input-method)
(pyim-basedict-enable)

;; insert space between Chinese and english character
(straight-use-package 'pangu-spacing)

(global-pangu-spacing-mode 1)
(setq pangu-spacing-real-insert-separtor t) ;; really insert the space

(provide 'init-cjk)
