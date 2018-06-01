;; -*- lexical-binding: t; -*-

(require 'org)

(let ((min-org-version "9.1"))
  (when (version< org-version min-org-version)
    (error "This org package (%s) is too old! Upgrade to %s or newer"
           org-version
           min-org-version)))

;; Keep your org files in a dedicated directory

(setq org-directory (expand-file-name "~/org"))

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

(setq org-refile-targets
      '((nil :maxlevel . 3) ;; nil = current buffer
        (org-agenda-files :level . 1)))

;; Along with TODO and DONE, define a NEXT state, so that you can
;; easily mark tasks that you plan to do in the near future (today).

(setq org-todo-keywords
      '((sequence "WAIT" "TODO" "NEXT" "|" "DONE")))

(setq org-todo-keyword-faces
      '(("WAIT" . "darkblue")
        ("TODO" . "darkred")
        ("NEXT" . "red")
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
               (setq org-map-continue-from (point))
               (org-archive-subtree))
             "/DONE"
             'file))))

;; Change all NEXT items back to TODO.

(defun my/org-reset-next-tasks ()
  "Change all items with NEXT status back to TODO."
  (interactive)
  (message "Reset %d tasks."
           (length
            (org-map-entries
             '(org-todo "TODO")
             "/NEXT"
             'file))))

(defun my/org-consume-property (property-name func scope)
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
  (let ((year-month (format-time-string "%Y-%m-")))
    (my/org-consume-property "deadline_day"
                             (lambda (day)
                               (org-deadline nil (concat year-month day)))
                             'file)
    (my/org-consume-property "scheduled_day"
                             (lambda (day)
                               (org-schedule nil (concat year-month day)))
                             'file)))

(defvar my/org-template-directory
  (expand-file-name "templates" org-directory)
  "Directory with my Org template files.")

(defun my/org-append-template (file-name outline-path)
  "Insert contents of FILE-NAME under the headline specified by OUTLINE-PATH."
  (goto-char (org-find-olp outline-path :current-buffer))
  (org-get-next-sibling)
  (insert-file-contents (expand-file-name file-name
                                          my/org-template-directory)))

(defun my/org-copy-daily ()
  "Copy daily recurring tasks into plan.org."
  (interactive)
  (my/org-append-template "daily.org" '("Today")))

(defun my/org-copy-weekly ()
  "Copy weekly recurring tasks into plan.org."
  (interactive)
  (my/org-append-template "weekly.org" '("This Week")))

(defun my/org-copy-monthly ()
  "Copy monthly recurring tasks into plan.org, setting deadlines as needed."
  (interactive)
  (my/org-append-template "monthly.org" '("This Month"))
  (my/org-set-dates))

(provide 'init-org)
