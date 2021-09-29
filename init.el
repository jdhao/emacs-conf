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

;;
;; some packages to install and their settings
;;

;; fancy icons: we need to install those icons font using command all-the-icons-install-fonts,
;; on Windows, this command may fail, so we need to install those fonts manually.
(straight-use-package 'all-the-icons)

;; fancy stratup screen
(straight-use-package 'dashboard)
;; start dashboard
(dashboard-setup-startup-hook)

;; doom-themes: a collection of themes
(straight-use-package 'doom-themes)
(straight-use-package 'zenburn-theme)
(straight-use-package 'solarized-theme)
(straight-use-package 'gruvbox-theme)
(straight-use-package 'material-theme)
(straight-use-package 'monokai-theme)

;; (load-theme 'doom-gruvbox t nil)
(load-theme 'solarized-dark t nil)
;; (load-theme 'zenburn t nil)
;; (load-theme 'gruvbox t nil)
;; (load-theme 'material t nil)
;; (load-theme 'monokai t nil)

;; ivy: fuzzy finder
(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'ivy-posframe)

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
      '((left-fringe . 4)
        (right-fringe . 4)))

(ivy-posframe-mode 1)

;; change ivy posframe background color to make it stand out, see
;; https://www.reddit.com/r/emacs/comments/jlsass/comment/gas9jaz/?utm_source=share&utm_medium=web2x&context=3
(set-face-attribute 'ivy-posframe nil
                    :foreground "white"
                    ;; list of colors: http://xay-lab.nautilus.xyz/2010/09/emacs.html
                    :background "#383838")

;; auto-completion with company
(straight-use-package 'company)

;; enable company mode in all buffers
(global-company-mode)

;; use tab and s-tab to cycle forward and backward completion items
(add-hook 'after-init-hook 'company-tng-mode)

;; minimum num of typed character to trigger auto-completion
(setq company-minimum-prefix-length 1)

;; insert space between Chinese and english character
(straight-use-package 'pangu-spacing)

(global-pangu-spacing-mode 1)
(setq pangu-spacing-real-insert-separtor t) ;; really insert the space

;; prescient: sort by usage frequency
(straight-use-package 'prescient)
(straight-use-package 'ivy-prescient)
(straight-use-package 'company-prescient)

(ivy-prescient-mode t)
(company-prescient-mode t)
(prescient-persist-mode t)

;; evil: vim emulation its extension packages
(straight-use-package 'evil)
(straight-use-package 'evil-anzu)
(straight-use-package 'evil-matchit)
(straight-use-package 'evil-commentary)

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

;; markdown mode
(straight-use-package 'markdown-mode)

;; spaceline: a beautiful mode line
(straight-use-package 'spaceline)
;; (straight-use-package 'spaceline-all-the-icons)

;; spaceline settings
(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
(setq powerline-default-separator 'slant)  ;; separator style

(spaceline-spacemacs-theme)
;; Diable showing buffer size, should be placed below the
;; spaceline-spacemacs-theme command.
(spaceline-toggle-buffer-size-off)

;; use fancy icons on mode line (currently does not work on Windows)
;; (spaceline-all-the-icons-theme)

;; smartparens: auto pair insertion
(straight-use-package 'smartparens)

;; enable smartparens mode
(smartparens-global-mode t)

;; gcmh: auto-gc mode
(straight-use-package 'gcmh)
(gcmh-mode 1)

;; faster window jump
(straight-use-package 'ace-window)

;; key binding for ace-window
(global-set-key (kbd "M-o") 'ace-window)

;; do not litter .emacs.d dir (TODO: settings?)
(straight-use-package 'no-littering)

;; show git gutter on left side
(straight-use-package 'git-gutter)

;; git-gutter settings

(when (string-equal system-type "darwin")
    ;; the following does not work for Windows since there is no diff executable
    (custom-set-variables
    '(git-gutter:update-interval 1)))

(custom-set-variables
    '(git-gutter:hide-gutter t))

(global-git-gutter-mode +1)

;; mappings for git-gutter
(global-set-key (kbd "C-x ]") 'git-gutter:next-hunk)
(global-set-key (kbd "C-x [") 'git-gutter:previous-hunk)

;; linting
(straight-use-package 'flycheck)
;; enable flycheck
;; (global-flycheck-mode)

;; orgmode
(when (string-equal system-type "darwin")
  (straight-use-package 'org-mode))

;; powerful git client
(straight-use-package 'magit)

;; faster buffer jump (like vim-sneak)
(straight-use-package 'avy)
;; load avy mode
(global-set-key (kbd "C-:") 'avy-goto-char-2)

;; hint key combinations
(straight-use-package 'which-key)
;; enable which-key
(setq which-key-idle-delay 0.5)
(which-key-mode t)

;; auto-save when focus lost
(straight-use-package 'super-save)
;; enable super-save
(super-save-mode +1)

(setq super-save-auto-save-when-idle t)

;; highlight lisp symbols
(straight-use-package 'highlight-defined)
;; enable hihglight-defined
(add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)

;; 输入中文
(straight-use-package 'pyim)
(straight-use-package 'pyim-basedict)
(require 'pyim)
(require 'pyim-basedict)

;; 显示 5 个候选词。
(setq pyim-page-length 9)
(setq-default default-input-method "pyim")

(pyim-default-scheme 'quanpin)
(global-set-key (kbd "C-\\") 'toggle-input-method)
(pyim-basedict-enable)

;; more helpful help window
(straight-use-package 'helpful)

;; bind keys for helpful
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
;; Lookup the current symbol at point.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; customize how major and minor modes are displayed on modeline
(straight-use-package
 '(blackout :host github :repo "raxod502/blackout"))

;; do not show some mode on modeline using blackout
(blackout 'eldoc-mode)
(blackout 'gcmh-mode)
(blackout 'evil-commentary-mode)
(blackout 'git-gutter-mode)
(blackout 'anzu-mode)
(blackout 'ivy-posframe-mode)
(blackout 'ivy-mode)
(blackout 'smartparens-mode)
(blackout 'which-key-mode)
(blackout 'company-mode)
(blackout 'super-save-mode)
(blackout 'emacs-lisp-mode "Elisp")

;; treat all installed themes as safe
(setq custom-safe-themes t)

;; show file path on title
(setq frame-title-format
      `((buffer-file-name "%f" "%b")
        ,(format " - GNU Emacs %s" emacs-version)))

;; use tab to cycle through the completion item
(setq completion-cycle-threshold 1)

;;
;; builtin settings
;;

;; open init.el using custom shortcut,
;; ref: https://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
;; https://stackoverflow.com/q/22816304/6064933
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c I") #'find-user-init-file)

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
      ((string-equal system-type "darwin") (message "%s" "macOS")))

(set-fontset-font t nil "Symbola" nil 'append)

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
;; remove trailing white space on save, ref: https://emacs.stackexchange.com/a/33720/23435
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; do not recenter cursorline eagerly
(setq scroll-conservatively 20)

;; do not ring bell (it is annoying)
(setq ring-bell-function 'ignore)

;; highlight current line
(global-hl-line-mode t)

;; set the default encoding and file format to utf-8 and unix, ref: https://emacs.stackexchange.com/a/5781/23435
(setq-default buffer-file-coding-system 'utf-8-unix)
