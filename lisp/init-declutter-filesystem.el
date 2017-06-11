;; To avoid cluttering the file system with #auto-save# files and
;; backup~ files, we'll direct Emacs to store those in the system
;; temporary directory. The auto-save-file-list (which stores the list
;; of open files) will be saved there, too.

(defconst my-auto-save-directory
  (expand-file-name "emacs-auto-saves/" temporary-file-directory))
(make-directory my-auto-save-directory :parents)
(setq auto-save-file-name-transforms `((".*" ,my-auto-save-directory t)))

(setq auto-save-list-file-prefix
      (expand-file-name "emacs-auto-save-list-" temporary-file-directory))

(defconst my-backup-directory
  (expand-file-name "emacs-backups" temporary-file-directory))
(setq backup-directory-alist `(("." . ,my-backup-directory)))

(provide 'init-declutter-filesystem)
