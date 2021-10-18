
;; linting
(straight-use-package 'flycheck)
;; enable flycheck
;; (global-flycheck-mode)

;; ref: https://www.reddit.com/r/emacs/comments/ku4u0u/delay_lsp_mode_diagnostics_until_saving/
(setq flycheck-check-syntax-automatically '(save))

;; go to next and previous error
(define-key evil-normal-state-map (kbd "] d") 'flycheck-next-error)
(define-key evil-normal-state-map (kbd "[ d") 'flycheck-previous-error)

;; change the flycheck fringe symbol, it is fucking hard compared to vim.
;; Ref: 1. https://github.com/syl20bnr/spacemacs/blob/63056ecb50f93808781b97feab1c3225d35c7aa1/layers/%2Bcheckers/syntax-checking/packages.el#L64-L102
;; 2. https://emacs.stackexchange.com/a/36373/23435
;; 3. https://www.gnu.org/software/emacs/manual/html_node/elisp/Fringe-Bitmaps.html
(define-fringe-bitmap 'error-bitmap
  (vector #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b01100110
          #b00100100
          #b00011000
          #b00011000
          #b00111100
          #b01100110
          #b01000010
          #b00000000
          #b00000000
          #b00000000
          #b00000000))

(define-fringe-bitmap 'warning-bitmap
  (vector  #b00000000
           #b00111000
           #b00111000
           #b00111000
           #b00111000
           #b00111000
           #b00111000
           #b00111000
           #b00111000
           #b00111000
           #b00000000
           #b00000000
           #b00111000
           #b01111100
           #b00111000
           #b00010000))


(define-fringe-bitmap 'info-bitmap
  (vector  #b00010000
           #b01111100
           #b00111000
           #b00000000
           #b00000000
           #b00111000
           #b00111000
           #b00111000
           #b00111000
           #b00111000
           #b00111000
           #b00111000
           #b00111000
           #b00111000
           #b00000000
           #b00000000))

(flycheck-define-error-level 'error
  :severity 2
  :overlay-category 'flycheck-error-overlay
  :fringe-bitmap 'error-bitmap
  :error-list-face 'flycheck-error-list-error
  :fringe-face 'flycheck-fringe-error)

(flycheck-define-error-level 'warning
  :severity 1
  :overlay-category 'flycheck-warning-overlay
  :fringe-bitmap 'warning-bitmap
  :error-list-face 'flycheck-error-list-warning
  :fringe-face 'flycheck-fringe-warning)

(flycheck-define-error-level 'info
  :severity 0
  :overlay-category 'flycheck-info-overlay
  :fringe-bitmap 'info-bitmap
  :error-list-face 'flycheck-error-list-info
  :fringe-face 'flycheck-fringe-info)

(straight-use-package 'flycheck-aspell)
(require 'flycheck-aspell)

;; If you want to check TeX/LaTeX/ConTeXt buffers
(add-to-list 'flycheck-checkers 'tex-aspell-dynamic)
;; If you want to check Markdown/GFM buffers
(add-to-list 'flycheck-checkers 'markdown-aspell-dynamic)
;; If you want to check HTML buffers
(add-to-list 'flycheck-checkers 'html-aspell-dynamic)
;; If you want to check XML/SGML buffers
(add-to-list 'flycheck-checkers 'xml-aspell-dynamic)
;; If you want to check Nroff/Troff/Groff buffers
(add-to-list 'flycheck-checkers 'nroff-aspell-dynamic)
;; If you want to check Texinfo buffers
(add-to-list 'flycheck-checkers 'texinfo-aspell-dynamic)
;; If you want to check comments and strings for C-like languages
(add-to-list 'flycheck-checkers 'c-aspell-dynamic)
;; If you want to check message buffers
(add-to-list 'flycheck-checkers 'mail-aspell-dynamic)

(provide 'init-lint)
