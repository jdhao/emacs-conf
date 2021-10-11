
;; search key words in the browser
(straight-use-package 'engine-mode)
(engine-mode t)

;; use ctrl-x-/-g to search
(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  :keybinding "g")

;; imemu-list: emacs's tagbar?
(straight-use-package 'imenu-list)
(setq imenu-list-auto-resize nil)

(global-set-key (kbd "C-'") 'imenu-list-smart-toggle)

;; right click menu
(straight-use-package 'mouse3)
(global-set-key (kbd "<mouse-3>") 'mouse3-popup-menu)

;; gcmh: auto-gc mode
(straight-use-package 'gcmh)
(gcmh-mode 1)

;; hint key combinations
(straight-use-package 'which-key)
;; enable which-key
(setq which-key-idle-delay 0.8)
(which-key-mode t)

;; show key pressed
;; (straight-use-package 'showkey)
;; (showkey-log-mode)

;; auto-save when focus lost
(straight-use-package 'super-save)
;; enable super-save
(super-save-mode +1)

(setq super-save-auto-save-when-idle t)

;; more helpful help window
(straight-use-package 'helpful)

;; bind keys for helpful
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
;; Lookup the current symbol at point.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; fix maxOS PATH issue when starting from GUI
(straight-use-package 'exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(provide 'init-misc)
