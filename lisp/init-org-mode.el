;; -*- lexical-binding: t; -*-

(require 'org)
(require 'org-agenda)

;; Use the standard key bindings

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c b") #'org-iswitchb)

;; You keep your .org files in Dropbox, for now...

(setq org-directory (expand-file-name "~/Dropbox/Documents"))

;; All .org files in the org-directory will be part of the Agenda. You
;; should consider limiting this to a fixed set of files (e.g., inbox,
;; plan, someday, calendar...).

(setq org-agenda-files (list org-directory))

;; Set the target file for notes

(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))

;; Add a timestamp when closing items

(setq org-log-done 'time)

;; On startup, display the agenda for the current week instead of the
;; default splash screen.

(setq inhibit-splash-screen t)

(add-hook 'after-init-hook
          (lambda ()
            (let ((org-agenda-window-setup 'current-window))
              (org-agenda-list))))

;; Where are we allowed to refile items?

(setq org-refile-targets '((nil :maxlevel . 2) ;; nil = current buffer
			   (org-agenda-files :level . 1)))

;; Along with TODO and DONE, define a NEXT state, so that you can
;; easily mark tasks that you plan to do in the near future (today).

(setq org-todo-keywords
      '((sequence "WAIT" "TODO" "NEXT" "|" "DONE")))

(setq org-todo-keyword-faces
      '(("TODO" . "darkred")
        ("WAIT" . "darkblue")
        ("NEXT" . org-warning) ; NEXT should pop!
	("DONE" . "darkgreen")))

;; Since you're using NEXT to identify tasks you plan to handle next,
;; let's define a custom command to quickly view them all. (And do the
;; same for WAIT items, too.)

(setq org-agenda-custom-commands
      `(("x" "Ne[x]t actions" tags "/NEXT")
        ("w" "What are you [w]aiting for?" tags "/WAIT")))

;; Here's a custom function (courtesy https://stackoverflow.com/a/27043756)
;; that automagically moves all DONE items to the file's archive file.

(defun my/org-archive-done-tasks ()
  "Automagically move all DONE items to the archive."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE"
   'file))

;; Here's a function to refile to a specified file and headline. The
;; third parameter to org-refile is a weird, poorly documented form.
;; https://emacs.stackexchange.com/q/8045

(defun my/org-refile-to (file-name headline)
  "Refile the headline at the point to the specified FILE-NAME and HEADLINE."
  (org-refile nil nil (list
		       headline
		       file-name
		       nil ;; regexp, unused
		       (marker-position
			(org-find-olp (list file-name headline))))))

(defun my/org-copy-recurring (template-tag target-headline)
  "Copy items with TEMPLATE-TAG from the recurring.org template file to
TARGET-HEADLINE in the plan.org file."
  (let ((template-file-name (expand-file-name "org-templates/recurring.org"
					      org-directory))
	(target-file-name (expand-file-name "plan.org" org-directory))
	(org-refile-keep t))
    (org-map-entries
     (lambda () (my/org-refile-to target-file-name target-headline))
     (concat template-tag "/TODO")
     (list template-file-name))))

(defun my/org-consume-property (property-name scope func)
  "Call FUNC at each headline with a value for PROPERTY-NAME in
SCOPE. After FUNC is called, the property is removed and the
properties drawer will also be removed if it is empty."
  (org-map-entries
   (lambda ()
     (let ((property-value (org-entry-get (point) property-name)))
       (funcall func property-value)
       (org-entry-delete nil property-name)
       (org-remove-empty-drawer-at (point))))
   (concat property-name "<>\"\"")
   scope))

(defun my/org-set-dates ()
  "Find TODO items with a `deadline_day` or `scheduled_day`
property, and set a DEADLINE or SCHEDULED date on that day in the
current year and month."
  (let ((yyyy-mm- (format-time-string "%Y-%m-"))
        (file-list (list (expand-file-name "plan.org" org-directory))))
    (my/org-consume-property "deadline_day"
                             file-list
                             (lambda (day)
                               (org-deadline nil (concat yyyy-mm- day))))
    (my/org-consume-property "scheduled_day"
                             file-list
                             (lambda (day)
                               (org-schedule nil (concat yyyy-mm- day))))))

(defun my/org-copy-daily ()
  "Copy daily recurring tasks into plan.org."
  (interactive)
  (my/org-copy-recurring "daily" "Today"))

(defun my/org-copy-weekly ()
  "Copy weekly recurring tasks into plan.org."
  (interactive)
  (my/org-copy-recurring "weekly" "This Week"))

(defun my/org-copy-monthly ()
  "Copy monthly recurring tasks into plan.org, setting deadlines as needed."
  (interactive)
  (my/org-copy-recurring "monthly" "This Month")
  (my/org-set-dates))

(provide 'init-org-mode)
