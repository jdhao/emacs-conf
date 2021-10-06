
;; orgmode
(when is-mac
  (straight-use-package 'org-mode))

;; live preview of latex equation
(straight-use-package 'org-fragtog)
(add-hook 'org-mode-hook 'org-fragtog-mode)

;; hide elments dynamically
;; (straight-use-package 'org-appear)
;; (add-hook 'org-mode-hook 'org-appear-mode)

(provide 'init-org)
