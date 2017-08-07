;; -*- lexical-binding: t; -*-

(require 'init-org)
(require 'org-capture)

;; Set the target file for notes

(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))

;; key description type target template properties...

(defun my/journal-file-name ()
  (expand-file-name (format-time-string "journal/%Y-%m-journal.org")
                    org-directory))

(setq org-capture-templates
      '(("t" "Task (inbox)" entry (file "inbox.org")
         "* TODO %?%i")
        ("T" "Task (Today)" entry (file+olp my/org-plan-file "Today")
         "* NEXT %?%i")
        ("l" "Task with link (Inbox)" entry (file "inbox.org")
         "* TODO %?%i\n  %a")
        ("wl" "Wishlist Link" entry (file+olp "wishlist.org" "Unsorted")
         "* [[%^{Link}][%^{Title}]]")
        ("j" "Journal" entry (file my/journal-file-name)
         "* %<%A, %B %_d, %Y; %_I:%M%P>\n\n%i%?"
         :empty-lines 1
         :kill-buffer)))

;; Bind keys for quick access to some templates

(global-set-key (kbd "C-c j") (lambda ()
                                (interactive)
                                (org-capture nil "j")))

(global-set-key (kbd "C-c t") (lambda ()
                                (interactive)
                                (org-capture nil "t")))

(provide 'init-org-capture)
