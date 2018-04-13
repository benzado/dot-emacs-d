;; -*- lexical-binding: t; -*-

(package-install 'ledger-mode)

(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)

(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

(provide 'init-ledger)
