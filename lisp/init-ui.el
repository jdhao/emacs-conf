
;; fancy icons: we need to install those icons font using command all-the-icons-install-fonts,
;; on Windows, this command may fail, so we need to install those fonts manually.
(straight-use-package 'all-the-icons)

;; fancy stratup screen
(straight-use-package 'dashboard)
;; start dashboard
(dashboard-setup-startup-hook)

;; change recent file save location, see https://emacs.stackexchange.com/a/19714/23435
(setq recentf-save-file (recentf-expand-file-name "~/.cache/emacs/recentf"))

;; doom-themes: a collection of themes
(straight-use-package 'doom-themes)
(straight-use-package 'zenburn-theme)
(straight-use-package 'solarized-theme)
(straight-use-package 'gruvbox-theme)
(straight-use-package 'material-theme)
(straight-use-package 'monokai-theme)

;; load a random theme
(defvar custom-themes '(gruvbox zenburn monokai solarized-dark material solarized-selenized-light))

(defun random-color-theme ()
  "Load a random color theme."
  (interactive)
  (random t)
  (setq cur-theme (nth (random (length custom-themes)) custom-themes))
  (load-theme cur-theme t)
  (message "Loaded Theme: %s" (symbol-name cur-theme)))

(random-color-theme)

;; rainbow delimiters
(straight-use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; spaceline: a beautiful mode line
(straight-use-package 'spaceline)

;; spaceline settings
(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
(setq powerline-default-separator 'slant)  ;; separator style

(spaceline-spacemacs-theme)
;; Diable showing buffer size, should be placed below the
;; spaceline-spacemacs-theme command.
(spaceline-toggle-buffer-size-off)

;; show tabs and trailing whitespace, Ref: http://ergoemacs.org/emacs/whitespace-mode.html
(require 'whitespace)
(setq whitespace-display-mappings
   ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
  '((space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
    (newline-mark 10 [182 10]) ; 10 LINE FEED
    (tab-mark 9 [9655 9] [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
    ))

(setq whitespace-style '(face tabs trailing tab-mark))

;; change whitespace style
;; (set-face-attribute 'whitespace-tab nil
;;                     :background "#f0f0f0"
;;                     :foreground "#00a8a8"
;;                     :weight 'bold)
;; (set-face-attribute 'whitespace-trailing nil
;;                     :background "#e4eeff"
;;                     :foreground "#183bc8"
;;                     :weight 'normal)

(global-whitespace-mode t)

;; beautiful tablines
(straight-use-package 'centaur-tabs)

(centaur-tabs-mode t)

;; Ref: https://github.com/jixiuf/vmacs/blob/master/conf/conf-centaur-tabs.el#L15
(defun centaur-tabs-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules.
    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `projectile-project-p' with project name."
  (list
   (cond
    ((derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)
     "Term")
    ((string-match-p (rx (or
                          "\*Launch "
                          "*dap-"
                          ))
                     (buffer-name))
     "Debug")
    ((string-match-p (rx (or
                          "\*Async-native-compile-log\*"
                          "\*Helm"
                          "\*company-documentation\*"
                          "\*helm"
                          "\*eaf"
                          "\*eldoc"
                          "\*Launch "
                          "*dap-"
                          "*EGLOT "
                          "\*Flymake log\*"
                          "\*Help\*"
                          "\*Ibuffer\*"
                          "\*gopls::stderr\*"
                          "\*gopls\*"
                          "\*Compile-Log\*"
                          "*Backtrace*"
                          "*Package-Lint*"
                          "\*sdcv\*"
                          "\*tramp"
                          "\*lsp-log\*"
                          "\*tramp"
                          "\*ccls"
                          "\*vc"
                          "\*xref"
                          "\*Warnings*"
                          "magit-"
                          "\*Http"
                          "\*Verb"
                          "\*Org Agenda\*"
                          "\*Async Shell Command\*"
                          "\*Shell Command Output\*"
                          "\*Calculator\*"
                          "\*Calc "
                          "\*Flycheck error messages\*"
                          "\*Gofmt Errors\*"
                          "\*Ediff"
                          "\*sdcv\*"
                          "\*osx-dictionary\*"
                          "\*Messages\*"
              "\*dashboard\*"
              "\*scratch\*"
                          ))
                     (buffer-name))
     "Emacs")
    ;; ((not (vmacs-show-tabbar-p)) nil)
    (t "Common"))))

(setq centaur-tabs-style "wave")
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-set-close-button nil)
(setq centaur-tabs-gray-out-icons 'buffer)
(setq centaur-tabs-set-bar 'over)

(when is-mac
  (setq centaur-tabs-set-modified-marker t))
(when is-win
  (centaur-tabs-change-fonts "arial" 100))

(setq centaur-tabs-show-new-tab-button nil)
(setq centaur-tabs--buffer-show-groups nil)

(define-key evil-normal-state-map (kbd "g t") 'centaur-tabs-forward)
(define-key evil-normal-state-map (kbd "g T") 'centaur-tabs-backward)

;; highlight lisp symbols
(straight-use-package 'highlight-defined)
;; enable hihglight-defined
(add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)

(provide 'init-ui)
