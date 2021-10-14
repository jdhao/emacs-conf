
;; fancy icons: we need to install those icons font using command all-the-icons-install-fonts,
;; on Windows, this command may fail, so we need to install those fonts manually.
(straight-use-package 'all-the-icons)

;; fancy stratup screen
(straight-use-package 'dashboard)
;; start dashboard
(dashboard-setup-startup-hook)

(setq dashboard-startup-banner 'logo)
(setq dashboard-center-content t)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
                                                   :height 1.1
                                                   :v-adjust -0.05
                                                   :face 'font-lock-keyword-face))

;; change recent file save location, see https://emacs.stackexchange.com/a/19714/23435
(setq recentf-save-file (recentf-expand-file-name "~/.cache/emacs/recentf"))

;; doom-themes: a collection of themes
(straight-use-package 'doom-themes)
(straight-use-package 'zenburn-theme)
(straight-use-package 'solarized-theme)
(straight-use-package 'gruvbox-theme)
(straight-use-package 'material-theme)
(straight-use-package 'color-theme-sanityinc-tomorrow)
(straight-use-package 'moe-theme)
(straight-use-package 'atom-one-dark-theme)
(straight-use-package 'kaolin-themes)
(straight-use-package 'humanoid-themes)
(straight-use-package 'lab-themes)

;; load a random theme
(defvar light-themes '(solarized-light
                       gruvbox-light-soft
                       kaolin-valley-light
                       humanoid-light
                       lab-light)
  "light themes to use")

(defvar dark-themes '(gruvbox
                      zenburn
                      doom-monokai-pro
                      solarized-dark
                      material
                      moe-dark
                      sanityinc-tomorrow-eighties
                      kaolin-temple
                      atom-one-dark
                      lab-dark)
  "dark themes to use")

(defvar jdhao-themes (append light-themes dark-themes))

(defun random-color-theme ()
  "Load a random color theme."
  (interactive)
  (random t)
  (setq current-theme (seq-random-elt jdhao-themes))
  (load-theme current-theme t)
  (message "Loaded Theme: %s" (symbol-name current-theme)))

(random-color-theme)

;; rainbow delimiters
(straight-use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; ;; spaceline: a beautiful mode line
;; (straight-use-package 'spaceline)

;; ;; change spaceline git segment, ref: https://github.com/TheBB/spaceline/issues/20#issuecomment-150488572.
;; (defadvice vc-mode-line (after strip-backend () activate)
;;     (when (stringp vc-mode)
;;       (let ((gitlogo (replace-regexp-in-string "^ Git." "  " vc-mode)))
;;         (setq vc-mode gitlogo))))

;; ;; spaceline settings
;; (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
;; (setq powerline-default-separator 'bar)  ;; separator style

;; (spaceline-spacemacs-theme)
;; ;; Diable showing buffer size, should be placed below the
;; ;; spaceline-spacemacs-theme command.
;; (spaceline-toggle-buffer-size-off)

(straight-use-package 'doom-modeline)

(setq doom-modeline-bar-width 0
      doom-modeline-modal-icon nil
      )
(doom-modeline-mode 1)

(doom-modeline-def-modeline 'main
  '(modals vcs remote-host buffer-info matches parrot selection-info)
  '(misc-info minor-modes input-method major-mode buffer-encoding buffer-position process checker))

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

;; We do not want to enable whitespace-mode globally, since for
;; non-file buffers, tab may be used to align elements, ref:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Major-Modes.html
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'text-mode-hook 'whitespace-mode)

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

(setq centaur-tabs-style "bar")
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

(straight-use-package 'ns-auto-titlebar)
(when is-mac (ns-auto-titlebar-mode))

(straight-use-package 'minimap)
(setq minimap-window-location 'right)

;; modern style fringe indicator
(straight-use-package 'modern-fringes)
(modern-fringes-mode)
(modern-fringes-invert-arrows)

;; popwin manager
(straight-use-package 'popwin)
(popwin-mode 1)

(provide 'init-ui)
