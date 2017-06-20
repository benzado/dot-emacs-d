;; Welcome to my Emacs init file. It lives at ~/.emacs.d/init.el.

;; Fail right away if Emacs is simply too old

(let ((min-version "25.2"))
  (when (version< emacs-version min-version)
    (error "This Emacs (%s) is too old! Upgrade to %s or newer."
	   emacs-version
	   min-version)))

;; Add ~/.emacs.d/lisp to the load-path

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Define a constant to indicate we're on a Mac

(defconst *is-a-mac* (eq system-type 'darwin))

;; Direct the interactive 'customize' interface to make changes to a
;; separate file (and not mess around with this one).

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-elpa) ; calls (package-initialize)
(require 'init-declutter-filesystem)
(require 'init-org-mode)
(require 'init-git)
(require 'init-simplenote)

(when *is-a-mac*
  (require 'init-macintosh))

(require 'epa-file) ;; Gnu Privacy Guard

;; Load the customizations

(load custom-file)

;; This is what enables `emacsclient` to be used by git, etc.

(server-start)
