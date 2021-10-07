;; the system type variable
(defconst is-mac (string-equal system-type "darwin"))
(defconst is-linux (string-equal system-type "gnu/linux"))
(defconst is-win (string-equal system-type "windows-nt"))

;;;; builtin settings
(setq debug-on-error t)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq scroll-preserve-screen-position t) ;; preserve column position when scrolling
(setq scroll-margin 3) ;; equivalent of vim's scrolloff option
(setq scroll-conservatively 101)

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

(global-set-key (kbd "C-l") (lambda () (interactive) (redraw-frame nil)))

(defun refresh-file ()
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t)
  (message "refresh file successfully"))

(global-set-key [f5] 'refresh-file)

;; use ctrl-enter to eval expression
(define-key emacs-lisp-mode-map (kbd "<C-return>") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "<C-S-return>") 'eval-buffer)

;; show line numbers on the left side of a window
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

;; show relative number like vim
(setq display-line-numbers-type 'relative)

;; show line and column number on modeline
(setq line-number-mode t)
(setq column-number-mode t)

;; highlight current line
(global-hl-line-mode t)

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
;; (add-to-list 'default-frame-alist '(cursor-color . "pale green"))

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

;; do not ring bell (it is annoying)
(setq visible-bell t
 ring-bell-function 'ignore)

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

;; do not use tab for indentation
(setq-default indent-tabs-mode nil)

(provide 'init-basic)
