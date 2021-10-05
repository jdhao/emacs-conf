
;; evil: vim emulation its extension packages
(straight-use-package 'evil)
(straight-use-package 'evil-anzu)
(straight-use-package 'evil-matchit)
(straight-use-package 'evil-commentary)
(straight-use-package 'undo-tree)

;; make j and k respect visual line mode.
;; note that it breaks evil-commentry, see https://github.com/linktohack/evil-commentary/issues/23.
;; (setq evil-respect-visual-line-mode t)

;; enable evil mode
(evil-mode 1)

;; use undo-tree for redo,
;; https://www.reddit.com/r/emacs/comments/n1pibp/comment/gwei7fw/
(evil-set-undo-system 'undo-tree)
(global-undo-tree-mode 1)

;; mimic vim's ctrl-u for evil mode, see https://stackoverflow.com/q/14302171/6064933
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

;; close a window or quit emacs (if it is the only window).
;; count window num ref: https://emacs.stackexchange.com/q/3494/23435.
(defun quit-win-or-quit-emacs ()
  "quit win if there is more than 1 windows, otherwise quit emacs directly"
  (interactive)
    (if (> (count-windows) 1)
        (delete-window)
        (save-buffers-kill-terminal)))

(define-key evil-normal-state-map (kbd ", q") 'quit-win-or-quit-emacs)

;; save buffer
(define-key evil-normal-state-map (kbd ", w") 'save-buffer)

;; don't need C-n, C-p (inteferes with company-quickhelp)
(define-key evil-insert-state-map (kbd "C-n") nil)
(define-key evil-insert-state-map (kbd "C-p") nil)

;; continuous shift of block of text, ref: https://alexpeits.github.io/emacs.d/#orgaf53a91
(define-key evil-visual-state-map (kbd "<") #'(lambda ()
                                                (interactive)
                                                (progn
                                                    (call-interactively 'evil-shift-left)
                                                    (execute-kbd-macro "gv"))))

(define-key evil-visual-state-map (kbd ">") #'(lambda ()
                                                (interactive)
                                                (progn
                                                (call-interactively 'evil-shift-right)
                                                (execute-kbd-macro "gv"))))

;; show search count and index
(require 'evil-anzu)
(global-anzu-mode +1)

;; use spaceline to show anzu status, so we need to disable anzu's native modeline config
(setq anzu-cons-mode-line-p nil)

;; enable evil-matchit
(global-evil-matchit-mode 1)

;; enable evil-commentary mode
(evil-commentary-mode)

(provide 'init-evil)
