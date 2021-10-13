;; fix the issue that line number is missing when completion menu is near it,
;; see also https://github.com/company-mode/company-mode/issues/921.
;; (straight-use-package 'company-posframe)
;; (company-posframe-mode 1)

;; (straight-use-package 'company-box)
;; ;; enable company box
;; (add-hook 'company-mode-hook 'company-box-mode)

;; auto-completion with company
(straight-use-package 'company)

;; enable company mode in all buffers
(global-company-mode)

;; use tab and s-tab to cycle forward and backward completion items,
;; note that after enabling tng-mode, we can not use return to select
;; the completion item, just press SPC.
(add-hook 'after-init-hook 'company-tng-mode)

;; minimum num of typed character to trigger auto-completion
(setq company-minimum-prefix-length 2)

(setq company-idle-delay 0.1)

;; whether to show number for completion items so that users can use
;; meta key and number to insert
(setq company-show-quick-access 'nil)

(when is-mac
  (progn (straight-use-package 'company-emoji)
         ;; add company-emoji to company bankends
         (add-to-list 'company-backends 'company-emoji)))

;; fuzzy searching the company completion candidate
;; (straight-use-package 'company-fuzzy)
;; (straight-use-package 'liquidmetal)

;; (global-company-fuzzy-mode 1)

;; (setq company-fuzzy-sorting-backend 'liquidmetal)
;; (setq company-fuzzy-show-annotation t)

(provide 'init-autocomplete)
