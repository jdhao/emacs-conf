
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

;; gcmh: auto-gc mode
(straight-use-package 'gcmh)
(gcmh-mode 1)

;; hint key combinations
(straight-use-package 'which-key)
;; enable which-key
(setq which-key-idle-delay 0.8)
(which-key-mode t)

;; show key pressed
(straight-use-package 'keycast)

;; Show keycast on doom-modeline, ref:
;; https://github.com/seagle0128/doom-modeline/issues/122
;; https://github.com/tarsius/keycast/issues/7.
(with-eval-after-load 'keycast
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))

  (add-to-list 'global-mode-string '("" mode-line-keycast)))

(keycast-mode)

;; auto-save when focus lost
(straight-use-package 'super-save)
;; enable super-save
(super-save-mode +1)

(setq super-save-auto-save-when-idle t)

;; more helpful help window
(straight-use-package 'helpful)

(straight-use-package
 '(ghelp :host github :repo "casouri/ghelp"))
(require 'ghelp)

;; bind keys for helpful
(global-set-key (kbd "C-h f") #'ghelp-describe-function)
(define-key evil-normal-state-map (kbd ", h f") 'ghelp-describe-function)

(global-set-key (kbd "C-h v") #'ghelp-describe-variable)
(define-key evil-normal-state-map (kbd ", h v") 'ghelp-describe-variable)

(global-set-key (kbd "C-h k") #'ghelp-describe-key)
(define-key evil-normal-state-map (kbd ", h k") 'ghelp-describe-key)

;; Lookup the current symbol at point.
(global-set-key (kbd "C-c C-d") #'ghelp-describe-at-point)
(define-key evil-normal-state-map (kbd ", h d") 'ghelp-describe-at-point)

;; fix maxOS PATH issue when starting from GUI
(straight-use-package 'exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; benchmark Emacs startup time
(straight-use-package 'esup)

(provide 'init-misc)
