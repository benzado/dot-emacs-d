;; -*- lexical-binding: t; -*-

(require 'init-org)
(require 'init-dropbox)
(require 'org-mobile)

;; MobileOrg: Org Mode on the go!

;; I'm currently using Dropbox to synchronize these files, and I'm
;; slightly paranoid, so I encrypt them.

(setq org-mobile-directory
      (expand-file-name "Apps/MobileOrg" dropbox-directory))

(setq org-mobile-use-encryption t)

(setq org-mobile-inbox-for-pull (expand-file-name "inbox.org" org-directory))

(setq org-mobile-force-id-on-agenda-items nil) ; I don't like the
                                               ; default behavior of
                                               ; adding a PROPERTIES
                                               ; drawer to *every*
                                               ; item, but I reserve
                                               ; the right to change
                                               ; my mind if
                                               ; match-by-title leads
                                               ; to problems down the
                                               ; road...

;; If a file named .mobileorg-password exists in the org-directory,
;; use the contents of that file as the password, and also
;; automatically pull after init and push before quitting.

(let ((password-file (expand-file-name ".mobileorg-password" org-directory)))
  (if (file-readable-p password-file)
      (progn
        (setq org-mobile-encryption-password
              (with-temp-buffer
                (insert-file-contents-literally password-file)
                (buffer-substring-no-properties (point-min) (point-max))))
        (add-hook 'after-init-hook #'org-mobile-pull)
        (add-hook 'kill-emacs-hook #'org-mobile-push))
    (message "Can't find MobileOrg password at %s" password-file)))

(provide 'init-org-mobile)
