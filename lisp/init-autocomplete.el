;; auto-completion with company
(straight-use-package 'company)
(straight-use-package 'company-emoji)

;; enable company mode in all buffers
(global-company-mode)

;; use tab and s-tab to cycle forward and backward completion items,
;; note that after enabling tng-mode, we can not use return to select
;; the completion item, just press SPC.
(add-hook 'after-init-hook 'company-tng-mode)

;; minimum num of typed character to trigger auto-completion
(setq company-minimum-prefix-length 2)

(setq company-idle-delay 0.1)

;; use meta key and number to insert
(setq company-show-quick-access 'left)

;; add company-emoji to company bankends
(add-to-list 'company-backends 'company-emoji)

(provide 'init-autocomplete)
