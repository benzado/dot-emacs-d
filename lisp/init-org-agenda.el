;; -*- lexical-binding: t; -*-

(require 'init-org)
(require 'org-agenda)

;; All .org files in the org-directory will be part of the Agenda. You
;; should consider limiting this to a fixed set of files (e.g., inbox,
;; plan, someday, calendar...).

(setq org-agenda-files (list org-directory))

;; Since you're using NEXT to identify tasks you plan to handle next,
;; let's define a custom command to quickly view them all. (And do the
;; same for WAIT items, too.)

(setq org-agenda-custom-commands
      `(("x" "Ne[x]t actions" tags "/NEXT")
        ("w" "What are you [w]aiting for?" tags "/WAIT")))

;; On startup, display the agenda for the current week instead of the
;; default splash screen.

(setq inhibit-splash-screen t)

(add-hook 'after-init-hook
          (lambda ()
            (let ((org-agenda-window-setup 'current-window))
              (org-agenda-list))))

(provide 'init-org-agenda)
