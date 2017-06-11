;; Use the standard key bindings

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; You keep your .org files in Dropbox, for now...

(setq org-directory (expand-file-name "~/Dropbox/Documents"))

;; All .org files in the org-directory will be part of the Agenda. You
;; should consider limiting this to a fixed set of files (e.g., inbox,
;; plan, someday, calendar...).

(setq org-agenda-files (list org-directory))

(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))

;; You used to load plan.org on startup, but you thought that might
;; have been interfering with `emacsclient` in some situations. Also,
;; maybe it would be better to depend on the Agenda more?

;;(setq initial-buffer-choice (expand-file-name "plan.org")))

;; Add a timestamp when closing items

(setq org-log-done 'time)

;; Where are we allowed to refile items?

(setq org-refile-targets '((nil :maxlevel . 2) ;; nil = current buffer
			   (org-agenda-files :level . 1)))

;; Along with TODO and DONE, define a NEXT state, so that you can
;; easily mark tasks that you plan to do in the near future (today).

(setq org-todo-keywords
      '((sequence "TODO" "NEXT" "|" "DONE")))

(setq org-todo-keyword-faces
      '(("NEXT" . org-warning) ; NEXT should pop!
	("TODO" . "darkred")
	("DONE" . "darkgreen")))

;; Since you're using NEXT to identify tasks you plan to handle next,
;; let's define a custom command to quickly view them all.

(setq org-agenda-custom-commands
      `(("x" "Ne[x]t actions" tags "/NEXT")))

;; Here's a custom function (courtesy https://stackoverflow.com/a/27043756)
;; that automagically moves all DONE items to the file's archive file.

(defun my/org-archive-done-tasks ()
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
  (org-refile nil nil (list
		       headline
		       file-name
		       nil ;; regexp, unused
		       (marker-position
			(org-find-olp (list file-name headline))))))

;; Here's a function to copy items from the recurring template file to
;; the plan file.

(defun my/org-copy-recurring (template-tag target-headline)
  (let ((template-file-name (expand-file-name "org-templates/recurring.org"
					      org-directory))
	(target-file-name (expand-file-name "plan.org" org-directory))
	(org-refile-keep t))
    (org-map-entries
     (lambda () (my/org-refile-to target-file-name target-headline))
     (concat template-tag "/TODO")
     (list template-file-name))))

;; Finds TODO items with a deadline_day property, and sets a DEADLINE
;; for that day in the current year and month.

(defun my/org-set-deadlines ()
  (let ((yyyy-mm- (format-time-string "%Y-%m-")))
    (org-map-entries
     (lambda ()
       (let* ((deadline-day (org-entry-get (point) "deadline_day"))
	      (date-string (concat yyyy-mm- deadline-day)))
	 (org-deadline nil date-string)
	 (org-delete-property "deadline_day" "PROPERTIES")
	 (setq org-map-continue-from (outline-next-heading))))
     "deadline_day<>\"\""
     (list (expand-file-name "plan.org" org-directory)))))

;; Here are functions to copy the daily/weekly/monthly tasks into the plan.

(defun my/org-copy-daily ()
  (interactive)
  (my/org-copy-recurring "daily" "Today"))

(defun my/org-copy-weekly ()
  (interactive)
  (my/org-copy-recurring "weekly" "This Week"))

(defun my/org-copy-monthly ()
  (interactive)
  (my/org-copy-recurring "monthly" "This Month")
  (my/org-set-deadlines))

(provide 'init-org-mode)
