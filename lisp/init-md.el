
;; markdown mode
(straight-use-package 'markdown-mode)

(setq markdown-enable-math t)
(setq markdown-fontify-code-blocks-natively t)

(setq markdown-header-scaling t)

(straight-use-package 'texfrag)

(add-hook 'markdown-mode-hook #'texfrag-mode)
(setq texfrag-scale 0.8)

(setq texfrag-markdown-preview-image-links nil)
(setq preview-transparent-color "yellow")

;; texfrag use preview from auctex. Since we load texfrag
;; only when in markdown-mode, we need to wrap settings
;; in with-eval-after-load, or we will get errors.
;; Ref: https://emacs.stackexchange.com/a/62986/23435
(with-eval-after-load 'preview
  ;; we need to change this face, so that image is not shown in black,
  ;; see also https://tex.stackexchange.com/a/52584/114857 and
  ;; https://github.com/TobiasZawada/texfrag/issues/19.
  (set-face-attribute 'preview-reference-face nil
                      :background "#fbf3db"))

(straight-use-package 'olivetti)

(setq olivetti-body-width 0.6)
(setq olivetti-style 'fancy)

(add-hook 'olivetti-mode 'global-display-line-numbers-mode)

;; we need to install grip first with `pip install grip`.
(straight-use-package 'grip-mode)

(provide 'init-md)
