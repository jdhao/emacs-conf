;; the system type variable
(defconst is-mac (string-equal system-type "darwin"))
(defconst is-linux (string-equal system-type "gnu/linux"))
(defconst is-win (string-equal system-type "windows-nt"))

;;;; builtin settings
(setq debug-on-error t)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq scroll-preserve-screen-position t) ;; preserve column position when scrolling

;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; treat all installed themes as safe
(setq custom-safe-themes t)

;; visual line mode
(global-visual-line-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; show file path on title
(setq frame-title-format
      `((buffer-file-name "%f" "%b")
        ,(format " - GNU Emacs %s" emacs-version)))

;; use tab to cycle through the completion item
(setq completion-cycle-threshold 1)

;; open init.el using custom shortcut,
;; ref: https://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
;; https://stackoverflow.com/q/22816304/6064933
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c I") #'find-user-init-file)

;; make home and end behave
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

;; show line numbers on the left side of a window
(global-display-line-numbers-mode)

;; use ctrl-enter to eval expression
(define-key emacs-lisp-mode-map (kbd "<C-return>") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "<C-S-return>") 'eval-buffer)

;; show relative number like vim
(setq display-line-numbers-type 'relative)

;; show line and column number on modeline
(setq line-number-mode t)
(setq column-number-mode t)

;; show maximized window on startup
(setq frame-resize-pixelwise t
      x-frame-normalize-before-maximize t)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; disable scroll bar
(toggle-scroll-bar -1)

;; disable toolbar
(tool-bar-mode -1)

;; disable menu bar
(menu-bar-mode -1)

;; change font style and size
(cond (is-win (set-frame-font "DejaVuSansMono NF 9"))
      (is-mac (set-face-attribute 'default nil :height 130)))

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
(setq visible-bell t
 ring-bell-function 'ignore)

;; highlight current line
(global-hl-line-mode t)

;; save place -- move to the place I was last time I visited this file
(save-place-mode t)

;; blink the cursor forever
(setq-default blink-cursor-blinks 0)

;; set the default encoding and file format to utf-8 and unix,
;; ref: https://emacs.stackexchange.com/a/5781/23435
(setq-default buffer-file-coding-system 'utf-8-unix)

;; emoji support for macOS, ref: https://github.crookster.org/emacs27-from-homebrew-on-macos-with-emoji/
(if (version< "27.0" emacs-version)
           (set-fontset-font
            "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
         (set-fontset-font
          t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

;;;; some packages to install and their settings

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

;; fancy icons: we need to install those icons font using command all-the-icons-install-fonts,
;; on Windows, this command may fail, so we need to install those fonts manually.
(straight-use-package 'all-the-icons)

;; fancy stratup screen
(straight-use-package 'dashboard)
;; start dashboard
(dashboard-setup-startup-hook)

;; change recent file save location, see https://emacs.stackexchange.com/a/19714/23435
(setq recentf-save-file (recentf-expand-file-name "~/.cache/emacs/recentf"))

;; search key words in the browser
(straight-use-package 'engine-mode)
(engine-mode t)

;; use ctrl-x-/-g to search
(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  :keybinding "g")

;; doom-themes: a collection of themes
(straight-use-package 'doom-themes)
(straight-use-package 'zenburn-theme)
(straight-use-package 'solarized-theme)
(straight-use-package 'gruvbox-theme)
(straight-use-package 'material-theme)
(straight-use-package 'monokai-theme)

;; load a random theme
(defvar custom-themes '(gruvbox zenburn monokai solarized-dark material))

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

;; imemu-list: emacs's tagbar?
(straight-use-package 'imenu-list)
(setq imenu-list-auto-resize t)

(global-set-key (kbd "C-'") 'imenu-list-smart-toggle)

;; ivy: fuzzy finder
(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'ivy-posframe)
(straight-use-package 'ivy-rich)

;; enable ivy mode by default
(ivy-mode 1)

;; enable ivy-rich mode
(ivy-rich-mode t)

;; or enable counsel mode
; (counsel-mode 1)

;; rebind ctrl-x ctrl-b to use counsel-switch-buffer
(global-set-key (kbd "C-X C-B") 'counsel-switch-buffer)
(global-set-key (kbd "M-x") 'counsel-M-x)

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
(straight-use-package 'company-quickhelp)

;; enable company mode in all buffers
(global-company-mode)

;; enable company-quickhelp
(company-quickhelp-mode)

;; use tab and s-tab to cycle forward and backward completion items
(add-hook 'after-init-hook 'company-tng-mode)

;; minimum num of typed character to trigger auto-completion
(setq company-minimum-prefix-length 2)

(setq company-idle-delay 0.1)

;; use meta key and number to insert
(setq company-show-quick-access 'left)

;; insert space between Chinese and english character
(straight-use-package 'pangu-spacing)

(global-pangu-spacing-mode 1)
(setq pangu-spacing-real-insert-separtor t) ;; really insert the space

;; right click menu
(straight-use-package 'mouse3)
(global-set-key (kbd "<mouse-3>") 'mouse3-popup-menu)

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
;; close a window
(define-key evil-normal-state-map (kbd ", q") 'delete-window)
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

;; markdown mode
(straight-use-package 'markdown-mode)

(setq markdown-enable-math t)

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

;; smartparens: auto pair insertion
(straight-use-package 'smartparens)

;; enable smartparens mode
(smartparens-global-mode t)
;; disable auto insert for some keys in smartparen mode,
;; ref: https://github.com/Fuco1/smartparens/issues/932
(sp-pair "'" nil :actions :rem)

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

(when is-mac
    ;; the following does not work for Windows since there is no diff executable
    (custom-set-variables
    '(git-gutter:update-interval 1)))

(custom-set-variables
    '(git-gutter:hide-gutter t))

(global-git-gutter-mode +1)

;; mappings for git-gutter
(define-key evil-normal-state-map (kbd "] c") 'git-gutter:next-hunk)
(define-key evil-normal-state-map (kbd "[ c") 'git-gutter:previous-hunk)

;; linting
(straight-use-package 'flycheck)
;; enable flycheck
;; (global-flycheck-mode)

;; orgmode
(when is-mac
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
(setq which-key-idle-delay 0.8)
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

;; customize how major and minor modes are displayed on modeline
(straight-use-package
 '(blackout :host github :repo "raxod502/blackout"))

;; do not show some mode on modeline using blackout
;; for minor modes
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
(blackout 'undo-tree-mode)
(blackout 'visual-line-mode)
(blackout 'global-whitespace-mode)

;; for major modes
(blackout 'emacs-lisp-mode "Elisp")
