;; To avoid cluttering the file system with #auto-save# files and
;; backup~ files, we'll direct Emacs to store those in the system
;; temporary directory. The auto-save-file-list (which stores the list
;; of open files) will be saved there, too.

(let ((default-directory temporary-file-directory))
  (let ((my-auto-save-directory (expand-file-name "emacs-auto-saves/")))
    (make-directory my-auto-save-directory :parents)
    (setq auto-save-file-name-transforms `((".*" ,my-auto-save-directory t))))
  (setq auto-save-list-file-prefix (expand-file-name "emacs-auto-save-list-"))
  (let ((my-backup-directory (expand-file-name "emacs-backups")))
    (setq backup-directory-alist `(("." . ,my-backup-directory)))))

(provide 'init-declutter-filesystem)
