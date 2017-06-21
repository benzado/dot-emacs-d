;; Use Spotlight for searching

(setq locate-command "mdfind")

;; Update Terminal.app's title bar depending on the file being edited.
;; The path should be URI-escaped, hence `url-hexify-string`.
;; See /etc/bashrc_Apple_Terminal for more.

(defun my/update-mac-terminal-title-for-window (window &optional norecord)
  (unless window-system ; nil on text terminals
    (send-string-to-terminal
     (format "\e]6;%s\a"
	     (let ((path (buffer-file-name (window-buffer window))))
	       (if path
		   (concat "file://" (url-hexify-string path))
		   ""))))))

(advice-add 'select-window :after 'my/update-mac-terminal-title-for-window)

(provide 'init-macintosh)
