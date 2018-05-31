;; -*- lexical-binding: t; -*-

(require 'init-org)
(require 'org-agenda)

;; Limit the Agenda to files where something is definitely
;; happening. I used to include the whole org-directory, but that
;; resulted in Buffer Clutter™.

(setq org-agenda-files
      (let ((default-directory org-directory))
        (list (expand-file-name "plan.org")
              (expand-file-name "calendar.org")
              (expand-file-name "inbox.org")
              (expand-file-name "someday.org"))))

;; Since you're using NEXT to identify tasks you plan to handle next,
;; let's define a custom command to quickly view them all. (And do the
;; same for WAIT items, too.)

(setq org-agenda-custom-commands
      `(("x" "Ne[x]t actions, agenda, and waiting-for" ((todo "NEXT")
                                                        (agenda)
                                                        (todo "WAIT")))
        ("X" "What's ne[X]t?" ((todo "NEXT")))
        ("W" "What are you [W]aiting for?" ((todo "WAIT")))))

;; When summoning the Agenda...

(setq org-agenda-window-setup 'current-window) ; ...replace the
                                               ; current window
                                               ; instead of opening a
                                               ; new one that takes up
                                               ; most of the frame...

(setq org-agenda-start-day "-1d") ; ...start with yesterday...

(setq org-agenda-span 8) ; ...and show eight days.

;; On startup, display the agenda for the current week instead of the
;; default splash screen.

(setq inhibit-splash-screen t)

(add-hook 'after-init-hook #'org-agenda-list)

(provide 'init-org-agenda)
