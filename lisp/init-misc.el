
;; search key words in the browser
(straight-use-package 'engine-mode)
(engine-mode t)

;; use ctrl-x-/-g to search
(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  :keybinding "g")

;; imemu-list: emacs's tagbar?
(straight-use-package 'imenu-list)
(setq imenu-list-auto-resize t)

(global-set-key (kbd "C-'") 'imenu-list-smart-toggle)

;; right click menu
(straight-use-package 'mouse3)
(global-set-key (kbd "<mouse-3>") 'mouse3-popup-menu)

;; prescient: sort by usage frequency
(straight-use-package 'prescient)
(straight-use-package 'ivy-prescient)
(straight-use-package 'company-prescient)

(ivy-prescient-mode t)
(company-prescient-mode t)
(prescient-persist-mode t)

;; gcmh: auto-gc mode
(straight-use-package 'gcmh)
(gcmh-mode 1)

;; do not litter .emacs.d dir (TODO: settings?)
(straight-use-package 'no-littering)

;; hint key combinations
(straight-use-package 'which-key)
;; enable which-key
(setq which-key-idle-delay 0.8)
(which-key-mode t)

;; auto-save when focus lost
(straight-use-package 'super-save)
;; enable super-save
(super-save-mode +1)

(setq super-save-auto-save-when-idle t)

(provide 'init-misc)
