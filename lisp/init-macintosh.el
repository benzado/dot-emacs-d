;; -*- lexical-binding: t; -*-

;; Use Spotlight for searching

(setq locate-command "mdfind")

;; Use GNU coreutils, if installed.

(let ((gnubin-directory "/opt/local/libexec/gnubin"))
  (when (file-accessible-directory-p gnubin-directory)
    (message "Adding '%s' to exec-path." gnubin-directory)
    (add-to-list 'exec-path gnubin-directory)))

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

;; Make Cmd-W work like it does in browsers and other apps that
;; support multiple tabs: it closes the current Emacs window (like C-x
;; 0) unless it is the sole window, in which case it closes the frame.

(defun my/delete-selected-window-or-frame ()
  "If a frame contains multiple windows, delete the selected
window, otherwise, delete the entire frame."
  (interactive)
  (if (> (count-windows) 1)
      (delete-window)
    (delete-frame)))

(global-set-key (kbd "s-w") #'my/delete-selected-window-or-frame)

;; Make Cmd-<Arrow> do what it usually does.

(global-set-key (kbd "<s-up>")    #'beginning-of-buffer)
(global-set-key (kbd "<s-down>")  #'end-of-buffer)
(global-set-key (kbd "<s-left>")  #'move-beginning-of-line)
(global-set-key (kbd "<s-right>") #'move-end-of-line)

;; Make Alt-Up/Down skip over paragraphs.

(global-set-key (kbd "<M-up>")   #'backward-paragraph)
(global-set-key (kbd "<M-down>") #'forward-paragraph)

;; Make Cmd-Plus/Minus zoom text.

(global-set-key (kbd "s-+") #'text-scale-increase)
(global-set-key (kbd "s--") #'text-scale-decrease)

;; Make Cmd-/ toggle comment on a line in programming mods

(define-key prog-mode-map (kbd "s-/") #'comment-line)

;; Out of the box...
;; - Cmd-Z already does undo
;; - Cmd-F already does isearch
;; - Alt-Delete already deletes a word

;; Don't use the right alt/option key as a meta key, so it can still
;; be used to type accented characters.

(setq ns-right-alternate-modifier nil)

(provide 'init-macintosh)
