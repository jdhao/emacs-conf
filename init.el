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

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; some packages to install
(straight-use-package 'doom-themes)
(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'ivy-posframe)
(straight-use-package 'company)
(straight-use-package 'evil)
(straight-use-package 'markdown-mode)
(straight-use-package 'markdown-preview-mode)
(straight-use-package 'doom-modeline)
(straight-use-package 'smartparens)
(straight-use-package 'gcmh)

;; enable gcmh
(gcmh-mode 1)

;; enable doom modeline
(doom-modeline-mode 1)

(setq doom-modeline-height 15)

;; enable evil mode
(evil-mode 1)

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

;; rebind ctrl-x ctrl-f to use counsel-fzf
(global-set-key (kbd "C-X C-F") 'counsel-fzf)

;; rebind ctrl-x ctrl-b to use counsel-switch-buffer
(global-set-key (kbd "C-X C-B") 'counsel-switch-buffer)

;; (require 'ivy-posframe)
;; display at `ivy-posframe-style'
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))

(setq ivy-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))

(ivy-posframe-mode 1)

;; show line numbers
(global-display-line-numbers-mode)

;; show relative number like vim
(setq display-line-numbers-type 'relative)

;; show maximized window on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; disable scroll bar
(toggle-scroll-bar -1)

;; disable toolbar
(tool-bar-mode -1)

;; disable menu bar
(menu-bar-mode -1)

;; show tabline
(global-tab-line-mode t)

;; change font size
(set-face-attribute 'default nil :height 130)

;; show matching parentheses
(show-paren-mode t)

;; the style for matching parentheses
(set-face-background 'show-paren-match "#ff0000")
(set-face-attribute 'show-paren-match nil
            :weight 'bold :underline t :overline nil :slant 'normal)


;; show trailing white space
(setq-default show-trailing-whitespace t)

;; change backup settings
(setq backup-directory-alist '(("." . "~/.cache/emacs-backups")))
