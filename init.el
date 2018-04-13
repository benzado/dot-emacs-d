;; -*- lexical-binding: t; -*-

;; Welcome to my Emacs init file. It lives at ~/.emacs.d/init.el.

;; Fail right away if Emacs is simply too old

(let ((min-version "25.2"))
  (when (version< emacs-version min-version)
    (error "This Emacs (%s) is too old! Upgrade to %s or newer."
	   emacs-version
	   min-version)))

;; Add ~/.emacs.d/lisp to the load-path

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

;; Direct the interactive 'customize' interface to make changes to a
;; separate file (and not mess around with this one).

(setq custom-file (locate-user-emacs-file "custom.el"))

;; Load the customizations

(load custom-file :no-error)

;; Operating-system specific init

(when (eq system-type 'darwin)
  (require 'init-system-darwin))

(when (eq system-type 'windows-nt)
  (require 'init-system-windows-nt))

;; Now the standard stuff

(require 'init-elpa) ; calls (package-initialize)
(require 'init-sendmail)
(require 'init-declutter-filesystem)
(require 'init-disabled)
(require 'init-org)
(require 'init-org-agenda)
(require 'init-org-capture)
(require 'init-org-mobile)
(require 'init-magit)
(require 'init-misc)
(require 'init-neotree)
(require 'init-ledger)

(require 'epa-file) ;; Gnu Privacy Guard

;; More packages to install

(package-install 'ansible-vault)
(package-install 'yaml-mode)

;; This is what enables `emacsclient` to be used by git, etc.

(when window-system
  (setq server-use-tcp (eq window-system 'w32))
  (server-start))
