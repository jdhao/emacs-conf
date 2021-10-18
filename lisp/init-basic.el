;; the system type variable
(defconst is-mac (string-equal system-type "darwin"))
(defconst is-linux (string-equal system-type "gnu/linux"))
(defconst is-win (string-equal system-type "windows-nt"))

;; set user name and user email
(setq user-full-name "Jiedong Hao"
      user-mail-address "jdhao@hotmail.com")

;; Disalbe the annoying "For information about GNU Emacs ..." message,
;; ref: https://emacs.stackexchange.com/a/437/23435
(defun display-startup-echo-area-message ()
  (message nil))

;; disable echo area message (the following does not actually work)
;; (setq inhibit-startup-echo-area-message t)

;; disable welcome page and message
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

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
      `("%f  " ,(format-time-string "%Y-%m-%d %T")))

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

;; use ctrl-enter to eval expression. lisp-mode-shared-map is the parent for emacs-lisp-mode-map
;; lisp-interaction-mode-map, so we set the shortcut here to avoid duplication, ref:
;; https://emacs.stackexchange.com/q/3830/23435.
(define-key lisp-mode-shared-map (kbd "<C-return>") 'eval-last-sexp)
(define-key lisp-mode-shared-map (kbd "<C-S-return>") 'eval-buffer)

;; show line numbers on the left side of a window
(global-display-line-numbers-mode t)

;; Show relative number like vim. Note that on Windows, the screen may
;; flash due to this, which is really annoying, but current there is
;; no easy fix, ref: https://www.reddit.com/r/emacs/comments/fbb4mm/how_to_debug_flickering/fp2chh5/.
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

;; disable scroll bar, toolbar, menu bar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; change font style and size, see also http://ergoemacs.org/emacs/emacs_list_and_set_font.html.
;; (cond (is-win (set-frame-font "DejaVuSansMono NF 10"))
;;       (is-mac (set-frame-font "Iosevka Nerd Font 15")))
;; For change font style on Windows, ref: https://www.gnu.org/software/emacs/manual/html_node/emacs/Windows-Fonts.html#Windows-Fonts.
(cond (is-win (set-frame-font "Cascadia Code-10:weight=medium:antialias=subpixel"))
      (is-mac (set-frame-font "Iosevka Nerd Font 15")))

(set-fontset-font t nil "Symbola" nil 'append)

;; the character set to use can be found via command
;; list-character-sets. To set font for current session, we can also
;; use command menu-set-font interactively.
;; https://www.reddit.com/r/emacs/comments/8tz1r0/comment/e1bjce6/
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Modifying-Fontsets.html
;; https://www.emacswiki.org/emacs/FontSets
(when is-win
  (progn (set-fontset-font t 'gb18030
                           ;; Noto Sans CJK: https://www.google.com/get/noto/help/cjk/
                           (font-spec :family "Noto Sans SC"))
         (dolist (item '(("Noto Sans SC" . 1.1)))
           (add-to-list 'face-font-rescale-alist item))))

;; change cursor color, see https://stackoverflow.com/a/4643018/6064933
;; (add-to-list 'default-frame-alist '(cursor-color . "pale green"))

;; show matching parentheses
(show-paren-mode t)

;; it seems that we need to put customization of faces after load-theme. Otherwise, the
;; custom face settings will be overwritten. There is no hook for load-theme in emacs, unlike vim,
;; where we can use au ColorScheme * for such things.
;; Ref, 1. https://emacs.stackexchange.com/q/41229/23435
;;      2. https://emacs.stackexchange.com/q/22686/23435
;;      3. https://www.reddit.com/r/emacs/comments/5l0ri6/is_there_a_reason_there_is_no_afterloadthemehook/
;;      4. https://www.greghendershott.com/2017/02/emacs-themes.html
;; the style for matching parentheses

;; define a custom load-theme hook, ref:
;; https://github.com/tshu-w/.emacs.d/blob/2f0d005a7afd6224c437068dd0ef6bb4953c39be/lisp/editor-ui.el#L11
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun load-theme@after (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add 'load-theme :after #'load-theme@after)

(defun my-custom-face-conf()
  (interactive)
  (set-face-attribute 'show-paren-match nil :weight 'bold :underline t)
  ;; use italic for comment, function names and variable names, ref:
  ;; https://github.com/hlissner/emacs-doom-themes/issues/248
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  ;; (set-face-attribute 'font-lock-function-name-face nil :slant 'italic)
  ;; (set-face-attribute 'font-lock-variable-name-face nil :slant 'italic)
  )

(add-hook 'after-load-theme-hook 'my-custom-face-conf)

;; remove trailing white space on save, ref: https://emacs.stackexchange.com/a/33720/23435
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; change backup settings
(setq backup-directory-alist '(("." . "~/.cache/emacs/backup")))

;; disable auto-save mode
(setq auto-save-default nil)

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

;; set the default encoding and file format to utf-8 and unix, ref:
;; https://emacs.stackexchange.com/a/5781/23435
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; emoji support for macOS, ref: https://github.crookster.org/emacs27-from-homebrew-on-macos-with-emoji/
(if (version< "27.0" emacs-version)
           (set-fontset-font
            "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
         (set-fontset-font
          t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

;; tab key settings
(setq-default tab-width 4)

;; do not use tab for indentation
(setq-default indent-tabs-mode nil)

;; whether to show sign on left fringe for non-file lines.
(setq-default indicate-empty-lines nil)

;; fill-column is like vim's textwidth, ref:
;; https://www.mail-archive.com/implementations-list@lists.ourproject.org/msg01780.html
(setq fill-column 100)

;; recommened by lsp-mode for better performance, ref:
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024))

;; show clickable links, ref: https://emacs.stackexchange.com/a/30529/23435
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'text-mode-hook 'goto-address-prog-mode)

;; set margin
(setq left-margin-width 4)

(provide 'init-basic)
