(add-to-list 'load-path "/opt/local/share/git/contrib/emacs")

(require 'git)
(require 'git-blame)

(add-to-list 'auto-mode-alist '("\\COMMIT_EDITMSG\\'" . vc-git-log-edit-mode))

(provide 'init-git)
