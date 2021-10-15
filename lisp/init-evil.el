
;; evil: vim emulation its extension packages
(straight-use-package 'evil)
(straight-use-package 'undo-tree)

;; disable showing current state on echo area, ref: https://emacs.stackexchange.com/a/29163/23435.
(setq evil-insert-state-message nil)
(setq evil-visual-state-message nil)

;; make j and k respect visual line mode.
;; note that it breaks evil-commentry, see https://github.com/linktohack/evil-commentary/issues/23.
;; (setq evil-respect-visual-line-mode t)

(setq evil-want-Y-yank-to-eol t) ;; Y behaves like y$, ref: https://emacs.stackexchange.com/a/28848/23435.
(setq evil-want-C-u-scroll t) ;; use ctrl-u to scroll

;; Treat symbol as words so the hypen between is considered part of a word.
(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t))

;; enable evil mode
(evil-mode 1)

;; use undo-tree for redo,
;; https://www.reddit.com/r/emacs/comments/n1pibp/comment/gwei7fw/
(evil-set-undo-system 'undo-tree)
(global-undo-tree-mode 1)

;; center search after pressing n
(defun my-center-line (&rest _)
  (evil-scroll-line-to-center nil))

(advice-add 'evil-search-next :after #'my-center-line)

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

(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

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

;; make J remove leading comment symbol like what vim does
(evil-define-operator +evil-join-a (beg end)
  "Join the selected lines.
This advice improves on `evil-join' by removing comment delimiters when joining
commented lines, by using `fill-region-as-paragraph'.
From https://github.com/emacs-evil/evil/issues/606"
  :motion evil-line
  (let* ((count (count-lines beg end))
         (count (if (> count 1) (1- count) count))
         (fixup-mark (make-marker)))
    (dotimes (var count)
      (if (and (bolp) (eolp))
          (join-line 1)
        (let* ((end (line-beginning-position 3))
               (fill-column (1+ (- end beg))))
          (set-marker fixup-mark (line-end-position))
          (fill-region-as-paragraph beg end nil t)
          (goto-char fixup-mark)
          (fixup-whitespace))))
    (set-marker fixup-mark nil)))

(advice-add #'evil-join :override #'+evil-join-a)

(straight-use-package 'evil-anzu)
;; show search count and index
(require 'evil-anzu)
(global-anzu-mode +1)

;; use spaceline to show anzu status, so we need to disable anzu's native modeline config
(setq anzu-cons-mode-line-p nil)

(straight-use-package 'evil-matchit)

;; enable evil-matchit
(global-evil-matchit-mode 1)

;; use tab to jump between matching pairs
(define-key evil-normal-state-map (kbd "TAB") 'evilmi-jump-items)

(straight-use-package 'evil-commentary)
;; enable evil-commentary mode
(evil-commentary-mode)

(straight-use-package 'evil-surround)
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
