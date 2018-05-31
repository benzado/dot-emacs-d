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
      '(("t" "TODO to inbox"
         entry (file "inbox.org")
         "* TODO %?%i")
        ("l" "TODO with link to inbox"
         entry (file "inbox.org")
         "* TODO %?%i\n  %a")
        ("w" "link to add to wishlist"
         entry (file+olp "wishlist.org" "Unsorted")
         "* [[%^{Link}][%^{Title}]]")
        ("j" "personal journal entry"
         entry (file my/journal-file-name)
         "* %<%A, %B %_d, %Y; %_I:%M%P>\n\n%i%?"
         :empty-lines 1
         :kill-buffer)
        ("F" "news to share with family"
         entry (file "news-family.org")
         "* %<%A, %B %_d, %Y; %_I:%M%P>\n\n%i%?"
         :empty-lines 1)
        ("L" "news to share with LogCheck"
         entry (file "news-logcheck.org")
         "* %<%A, %B %_d, %Y; %_I:%M%P>\n\n%i%?"
         :empty-lines 1)))

;; Bind keys for quick access to some templates

(global-set-key (kbd "C-c j") (lambda ()
                                (interactive)
                                (org-capture nil "j")))

(global-set-key (kbd "C-c t") (lambda ()
                                (interactive)
                                (org-capture nil "t")))

(provide 'init-org-capture)
