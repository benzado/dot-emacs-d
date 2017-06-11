;; Use Spotlight for searching

(setq locate-command "mdfind")

;; Update Terminal.app's title bar depending on the file being edited.
;; In theory, the path should be URI-escaped, but that hasn't mattered
;; so far. See /etc/bashrc_Apple_Terminal

(defun my/update-mac-terminal-title-for-window (window &optional norecord)
  (unless window-system ; nil on text terminals
    (send-string-to-terminal
     (format "\e]6;%s\a"
	     (let ((path (buffer-file-name (window-buffer window))))
	       (if path
		   (concat "file://" path)
		   ""))))))

(advice-add 'select-window :after 'my/update-mac-terminal-title-for-window)

(provide 'init-macintosh)
