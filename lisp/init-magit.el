;; -*- lexical-binding: t; -*-

(package-install 'magit)

(require 'magit)

(global-set-key (kbd "C-x g") #'magit-status)
(global-set-key (kbd "C-x M-g") #'magit-dispatch-popup)

(provide 'init-magit)
