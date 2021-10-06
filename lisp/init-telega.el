
;; We should first install tdlib (brew install --HEAD tdlib), then install telega,
;; see also https://github.com/zevlg/telega.el/issues/104.
(when is-mac
  (straight-use-package 'telega))

;; telega proxies
(setq telega-proxies
      (list
       '(:server "127.0.0.1" :port 1089 :enable t
                 :type (:@type "proxyTypeSocks5"))
       ))

(provide 'init-telega)
