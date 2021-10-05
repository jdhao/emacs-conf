
;; smartparens: auto pair insertion
(straight-use-package 'smartparens)

;; enable smartparens mode
(smartparens-global-mode t)
;; disable auto insert for some keys in smartparen mode,
;; ref: https://github.com/Fuco1/smartparens/issues/932
(sp-pair "'" nil :actions :rem)

(provide 'init-parens)
