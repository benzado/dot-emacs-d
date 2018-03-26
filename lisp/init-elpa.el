;; -*- lexical-binding: t; -*-

;; M-x package-list-packages to browse and install

(require 'package)

;; Declare package sources. HTTPS doesn't work on Windows.

(let* ((no-https (and (eq system-type 'windows-nt)
		      (not (gnutls-available-p))))
       (proto (if no-https "http:" "https:")))
  (setq package-archives
	`(("gnu" . ,(concat proto "//elpa.gnu.org/packages/"))
	  ("melpa" . ,(concat proto "//melpa.org/packages/")))))

;; If a package exists in multiple repositories, the one with the
;; highest priority is used, even if it is an older version.

(setq package-archive-priorities
      '(("gnu" . 1)
	("melpa" . 0)))

(setq package-enable-at-startup nil) ; don't autoload

(package-initialize)

(provide 'init-elpa)
