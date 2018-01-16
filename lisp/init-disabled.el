;; -*- lexical-binding: t; -*-

;; If a command has a non-nil 'disabled property set on its Lisp
;; symbol, Emacs will prompt for confirmation when the user tries to
;; use the command. We override the default settings here.

;; See `Disabling Commands` in the Emacs manual for more detail.

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(provide 'init-disabled)
