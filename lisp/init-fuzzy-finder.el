
;; ivy: fuzzy finder
(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'ivy-rich) ;; show useful info for ivy items
(straight-use-package 'all-the-icons-ivy-rich) ;; add fancy icon to ivy UI

(all-the-icons-ivy-rich-mode 1)

;; enable ivy-rich mode
(ivy-rich-mode t)

;; enable ivy mode by default
(ivy-mode 1)

;; or enable counsel mode
(counsel-mode 1)

;; rebind ctrl-x ctrl-b to use counsel-switch-buffer
(global-set-key (kbd "C-X C-B") 'counsel-switch-buffer)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; (straight-use-package 'ivy-posframe)

;; display at `ivy-posframe-style'
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))

;; (setq ivy-posframe-parameters
;;       '((left-fringe . 4)
;;         (right-fringe . 4)))

;; (ivy-posframe-mode 1)

;; change ivy posframe background color to make it stand out, see
;; https://www.reddit.com/r/emacs/comments/jlsass/comment/gas9jaz/?utm_source=share&utm_medium=web2x&context=3
;; (set-face-attribute 'ivy-posframe nil
;;                     :foreground "white"
;;                     ;; list of colors: http://xay-lab.nautilus.xyz/2010/09/emacs.html
;;                     :background "#383838")

(provide 'init-fuzzy-finder)
