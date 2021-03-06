;; -*- lexical-binding: t; -*-

;; M-x package-refresh-contents to download the latest package lists.
;; M-x package-list-packages to browse and install

(require 'package)

;; Declare package sources. HTTPS doesn't work on Windows.

(let* ((no-https (and (eq system-type 'windows-nt)
		      (not (gnutls-available-p))))
       (proto (if no-https "http:" "https:")))
  (setq package-archives
	`(("org" . ,(concat proto "//orgmode.org/elpa/"))
	  ("gnu" . ,(concat proto "//elpa.gnu.org/packages/"))
          ("melpa" . ,(concat proto "//melpa.org/packages/")))))

;; If a package exists in multiple repositories, the one with the
;; highest priority is used, even if it is an older version.

(setq package-archive-priorities
      '(("org" . 2)
	("gnu" . 1)
        ("melpa" . 0)))

(setq package-enable-at-startup nil) ; don't autoload

(package-initialize)

(unless package-activated-list
  (message "Looks like a fresh install; refreshing available packages.")
  (package-refresh-contents))

(provide 'init-elpa)
