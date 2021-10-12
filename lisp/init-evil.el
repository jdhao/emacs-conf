
;; evil: vim emulation its extension packages
(straight-use-package 'evil)
(straight-use-package 'evil-anzu)
(straight-use-package 'evil-matchit)
(straight-use-package 'evil-commentary)
(straight-use-package 'undo-tree)
(straight-use-package 'evil-surround)

;; make j and k respect visual line mode.
;; note that it breaks evil-commentry, see https://github.com/linktohack/evil-commentary/issues/23.
;; (setq evil-respect-visual-line-mode t)

(setq evil-want-Y-yank-to-eol t) ;; Y behaves like y$, ref: https://emacs.stackexchange.com/a/28848/23435.
(setq evil-want-C-u-scroll t) ;; use ctrl-u to scroll

;; enable evil mode
(evil-mode 1)

;; use undo-tree for redo,
;; https://www.reddit.com/r/emacs/comments/n1pibp/comment/gwei7fw/
(evil-set-undo-system 'undo-tree)
(global-undo-tree-mode 1)

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

;; delete trailing space
(define-key evil-normal-state-map (kbd ", SPC") 'delete-trailing-whitespace)

;; redefine H and L
(define-key evil-normal-state-map (kbd "H") 'beginning-of-line)
(define-key evil-normal-state-map (kbd "L") 'end-of-line)

;; don't need C-n, C-p (inteferes with company-quickhelp)
(define-key evil-insert-state-map (kbd "C-n") nil)
(define-key evil-insert-state-map (kbd "C-p") nil)

;; In order to use jk to leave insert mode, see also https://stackoverflow.com/q/10569165/6064933.
(straight-use-package 'key-chord)

;; timeout between pressing of two keys (it is like vim's timeoutlen)
(setq key-chord-two-keys-delay 0.2)
(key-chord-mode 1)

(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

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

;; enable evil-surround
(global-evil-surround-mode)

;; show hint for some operation, e.g., yank
(straight-use-package 'evil-goggles)

;; highlight duration
(setq evil-goggles-duration 0.300)

(setq evil-goggles-pulse nil) ;whether to pulse

;; disable highlight for certain ops
(setq evil-goggles-enable-delete nil
      evil-goggles-enable-commentary nil)

(evil-goggles-mode)

(provide 'init-evil)
