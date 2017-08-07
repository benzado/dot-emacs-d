;; -*- lexical-binding: t; -*-

(require 'org)

;; Keep your org files in a dedicated directory

(setq org-directory (expand-file-name "~/org"))

(defvar my/org-plan-file
  (expand-file-name "plan.org" org-directory)
  "My main Org file.")

(defvar my/org-template-directory
  (expand-file-name "templates" org-directory)
  "Directory with my Org template files.")

;; Use the standard key bindings

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c b") #'org-iswitchb)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c l") #'org-store-link)

;; Bind C-c o _ to open selected org files

(let ((default-directory org-directory))
  (mapc (lambda (x)
          (let ((key-sequence (concat "C-c o " (car x)))
                (file (expand-file-name (cdr x))))
            (global-set-key (kbd key-sequence)
                            (lambda ()
                              (interactive)
                              (find-file file)))))
        '(("c" . "calendar.org")
          ("i" . "inbox.org")
          ("p" . "plan.org")
          ("q" . "quotations.org")
          ("r" . "reference.org")
          ("s" . "someday.org")
          ("w" . "wishlist.org"))))

;; Add a timestamp when closing items

(setq org-log-done 'time)

;; Where are we allowed to refile items?

(defun my/org-refile-target-file-names ()
  (directory-files org-directory :full-names "\\.org$" :no-sort))

(setq org-refile-targets '((nil :maxlevel . 3) ;; nil = current buffer
			   (my/org-refile-target-file-names :level . 1)))

;; Along with TODO and DONE, define a NEXT state, so that you can
;; easily mark tasks that you plan to do in the near future (today).

(setq org-todo-keywords
      '((sequence "WAIT" "TODO" "NEXT" "|" "DONE")))

(setq org-todo-keyword-faces
      '(("TODO" . "darkred")
        ("WAIT" . "darkblue")
        ("NEXT" . org-warning) ; NEXT should pop!
	("DONE" . "darkgreen")))

;; Here's a custom function (courtesy https://stackoverflow.com/a/27043756)
;; that automagically moves all DONE items to the file's archive file.

(defun my/org-archive-done-tasks ()
  "Automagically move all DONE items to the archive."
  (interactive)
  (message "Archived %d DONE tasks."
           (length
            (org-map-entries
             (lambda ()
               (org-archive-subtree)
               (setq org-map-continue-from (outline-previous-heading)))
             "/DONE"
             'file))))

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
  (let ((org-refile-keep t))
    (org-map-entries
     (lambda () (my/org-refile-to my/org-plan-file target-headline))
     (concat template-tag "/TODO")
     (list (expand-file-name "recurring.org" my/org-template-directory)))))

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
        (file-list (list my/org-plan-file)))
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

(provide 'init-org)
