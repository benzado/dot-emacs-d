;; -*- lexical-binding: t; -*-

;; Indent with spaces, not tabs.

(setq-default indent-tabs-mode nil)

;; Use Visual Line Mode when editing text

(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; Show trailing whitespace in programming modes.

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; Use iBuffer instead of Buffer List.

(autoload 'ibuffer "ibuffer")
(global-set-key [remap list-buffers] 'ibuffer)

(provide 'init-misc)
