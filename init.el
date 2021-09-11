(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; treat all installed themes as safe
(setq custom-safe-themes t)

(load-theme 'doom-gruvbox t nil)

;; move custom settings to another file, taken from https://github.com/purcell/emacs.d/blob/master/init.el
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

;; show file path on title
(setq frame-title-format
      `((buffer-file-name "%f" "%b")
        ,(format " - GNU Emacs %s" emacs-version)))

;; use tab to cycle through the completion item
(setq completion-cycle-threshold 1)

;; enable company mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)

;; show line numbers
(global-display-line-numbers-mode)

;; show relative number like vim
(setq display-line-numbers-type 'relative)
