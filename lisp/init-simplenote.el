;; See README at https://github.com/alpha22jp/simplenote2.el

(require 'simplenote2)

(setq simplenote2-email "ben@benzado.com"
      simplenote2-password nil) ; prompt for password

(simplenote2-setup)

(provide 'init-simplenote)
