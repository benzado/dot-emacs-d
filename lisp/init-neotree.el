;; -*- lexical-binding: t; -*-

(package-install 'neotree)

(require 'neotree)

;; https://github.com/jaypei/emacs-neotree

(global-set-key [f8] #'neotree-toggle)

(setq neo-theme 'nerd)

(provide 'init-neotree)
