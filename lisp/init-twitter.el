(require 'twittering-mode)

(setq twittering-use-master-password t)

(setq twittering-icon-mode t)

(when *is-a-mac*
  (let ((cache-directory (expand-file-name "~/Library/Caches/org.gnu.Emacs")))
    (make-directory cache-directory :parents)
    (let ((file (expand-file-name "twittering-mode-icons.gz" cache-directory)))
      (setq twittering-use-icon-storage t
            twittering-icon-storage-file file))))

(provide 'init-twitter)
