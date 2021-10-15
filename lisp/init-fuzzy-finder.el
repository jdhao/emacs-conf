
;; ivy: fuzzy finder
(straight-use-package 'ivy)
;; enable ivy mode by default
(ivy-mode 1)

;; ivy fuzzy searching, ref: https://oremacs.com/2016/01/06/ivy-flx/
;; (setq ivy-re-builders-alist
;;       '((t . ivy--regex-fuzzy)))

(straight-use-package 'counsel)
;; or enable counsel mode
(counsel-mode 1)

;; rebind ctrl-x ctrl-b to use counsel-switch-buffer
(global-set-key (kbd "C-X C-B") 'counsel-switch-buffer)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(straight-use-package 'ivy-rich) ;; show useful info for ivy items

;; enable ivy-rich mode
(add-hook 'ivy-mode-hook 'ivy-rich-mode)

(straight-use-package 'all-the-icons-ivy-rich) ;; add fancy icon to ivy UI
(all-the-icons-ivy-rich-mode 1)

(straight-use-package 'ivy-posframe)

;; display at `ivy-posframe-style'
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))

(setq ivy-posframe-parameters
      '((left-fringe . 1)
        (right-fringe . 1)))

(ivy-posframe-mode 1)

;; change ivy-posframe border color, ref: https://github.com/tumashu/ivy-posframe/issues/83.
;; we can show Emacs builtin colors via command list-colors-display.
(set-face-attribute 'ivy-posframe-border nil
                    :background "skyblue")

;; change ivy posframe background color to make it stand out, see
;; https://www.reddit.com/r/emacs/comments/jlsass/comment/gas9jaz/?utm_source=share&utm_medium=web2x&context=3
;; (set-face-attribute 'ivy-posframe nil
;;                     :foreground "white"
;;                     ;; list of colors: http://xay-lab.nautilus.xyz/2010/09/emacs.html
;;                     :background "#383838")

;; prescient can take the place of flx,
;; ref: https://github.com/raxod502/prescient.el/issues/13
(straight-use-package 'prescient)
(straight-use-package 'ivy-prescient)

(setq prescient-filter-method '(initialism fuzzy))
(setq prescient-sort-full-matches-first t)

(setq ivy-prescient-retain-classic-highlighting t)
(ivy-prescient-mode 1)

(prescient-persist-mode)

(provide 'init-fuzzy-finder)
