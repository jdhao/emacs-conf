;; install straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; some packages to install
(straight-use-package 'doom-themes)
(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'ivy-posframe)
(straight-use-package 'company)
(straight-use-package 'evil)
(straight-use-package 'evil-anzu)
(straight-use-package 'evil-matchit)
(straight-use-package 'evil-commentary)
(straight-use-package 'markdown-mode)
(straight-use-package 'spaceline)
(straight-use-package 'smartparens)
(straight-use-package 'gcmh)
(straight-use-package 'ace-window)
(straight-use-package 'all-the-icons)
(straight-use-package 'dashboard)
(straight-use-package 'no-littering)
(straight-use-package 'git-gutter)
(straight-use-package 'flycheck)
(straight-use-package 'org-mode)
(straight-use-package 'magit)

;; spaceline settings

(spaceline-spacemacs-theme)
(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
(setq powerline-default-separator 'slant)  ;; separator style

;; git-gutter settings
(global-git-gutter-mode +1)

;; mappings for git-gutter
(global-set-key (kbd "C-x ]") 'git-gutter:next-hunk)
(global-set-key (kbd "C-x [") 'git-gutter:previous-hunk)

;; start dashboard
(dashboard-setup-startup-hook)

;; key binding for ace-window
(global-set-key (kbd "M-o") 'ace-window)

;; enable gcmh
(gcmh-mode 1)

;; evil settings

;; enable evil mode
(evil-mode 1)

;; mimic vim's ctrl-u for evil mode, see https://stackoverflow.com/q/14302171/6064933
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

;; show search count and index
(require 'evil-anzu)
(global-anzu-mode +1)

;; use spaceline to show anzu status, so we need to disable anzu's native modeline config
(setq anzu-cons-mode-line-p nil)

;; enable evil-matchit
(global-evil-matchit-mode 1)

;; enable evil-commentary mode
(evil-commentary-mode)

;; enable smartparens mode
(smartparens-global-mode t)

;; treat all installed themes as safe
(setq custom-safe-themes t)

(load-theme 'doom-gruvbox t nil)

;; show file path on title
(setq frame-title-format
      `((buffer-file-name "%f" "%b")
        ,(format " - GNU Emacs %s" emacs-version)))

;; use tab to cycle through the completion item
(setq completion-cycle-threshold 1)

;; enable company mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)

;; use tab and s-tab to cycle forward and backward completion items
(add-hook 'after-init-hook 'company-tng-mode)

;; minimum num of typed character to trigger auto-completion
(setq company-minimum-prefix-length 1)

;; enable ivy mode by default
(ivy-mode 1)

;; or enable counsel mode
; (counsel-mode 1)

;; rebind ctrl-x ctrl-b to use counsel-switch-buffer
(global-set-key (kbd "C-X C-B") 'counsel-switch-buffer)

;; (require 'ivy-posframe)
;; display at `ivy-posframe-style'
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))

(setq ivy-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))

(ivy-posframe-mode 1)

;; enable flycheck
;; (global-flycheck-mode)

;;
;; builtin settings
;;

;; show line numbers on the left side of a window
(global-display-line-numbers-mode)

;; show relative number like vim
(setq display-line-numbers-type 'relative)

;; show line and column number on modeline
(setq line-number-mode t)
(setq column-number-mode t)

;; show maximized window on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; disable scroll bar
(toggle-scroll-bar -1)

;; disable toolbar
(tool-bar-mode -1)

;; disable menu bar
(menu-bar-mode -1)

;; tabline settings
;; packages to consider: https://github.com/ema2159/centaur-tabs
(global-tab-line-mode t)

;; customize the tab line looks, ref: https://amitp.blogspot.com/2020/06/emacs-prettier-tab-line.html
(setq tab-line-new-button-show nil)  ;; do not show add-new button
(setq tab-line-close-button-show nil)  ;; do not show close button
(setq tab-line-separator "")

(defvar my/tab-height 22)
(defvar my/tab-left (powerline-wave-right 'tab-line nil my/tab-height))
(defvar my/tab-right (powerline-wave-left nil 'tab-line my/tab-height))

(defun my/tab-line-tab-name-buffer (buffer &optional _buffers)
  (powerline-render (list my/tab-left
                          (format "%s" (buffer-name buffer))
                          my/tab-right)))
(setq tab-line-tab-name-function #'my/tab-line-tab-name-buffer)

;; tab color settings
(set-face-attribute 'tab-line nil ;; background behind tabs
      :background "gray40"
      :foreground "gray60" :distant-foreground "gray50"
      :height 1.0 :box nil)
(set-face-attribute 'tab-line-tab nil ;; active tab in another window
      :inherit 'tab-line
      :foreground "gray70" :background "gray90" :box nil)
(set-face-attribute 'tab-line-tab-current nil ;; active tab in current window
      :background "#b34cb3" :foreground "white" :box nil)
(set-face-attribute 'tab-line-tab-inactive nil ;; inactive tab
      :background "gray80" :foreground "black" :box nil)
(set-face-attribute 'tab-line-highlight nil ;; mouseover
      :background "white" :foreground 'unspecified)

;; change font style and size
(cond ((string-equal system-type "windows-nt") (set-frame-font "DejaVuSansMono NF 9"))
      ((string-equal system-type "darwin") (print "macOS"))
      )

;; change cursor color, see https://stackoverflow.com/a/4643018/6064933
(add-to-list 'default-frame-alist '(cursor-color . "pale green"))

;; show matching parentheses
(show-paren-mode t)

;; the style for matching parentheses
(set-face-background 'show-paren-match "#ff0000")
(set-face-attribute 'show-paren-match nil
            :weight 'bold :underline t :overline nil :slant 'normal)


;; show trailing white space
(setq-default show-trailing-whitespace t)

;; change backup settings
(setq backup-directory-alist '(("." . "~/.cache/emacs/backup")))

;; disable auto-save mode
(setq auto-save-default nil)

;; change recent file save location, see https://emacs.stackexchange.com/a/19714/23435
(setq recentf-save-file (recentf-expand-file-name "~/.cache/emacs/recentf"))

;; disable welcome page and message
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; disable echo area message
(setq inhibit-startup-echo-area-message t)

;; auto-reload file if it has been changed outside of Emacs, see
;; https://stackoverflow.com/q/1480572/6064933
(global-auto-revert-mode t)

;; set language environment
(set-language-environment "UTF-8")
