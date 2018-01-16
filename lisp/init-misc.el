;; -*- lexical-binding: t; -*-

;; Indent with spaces, not tabs.

(setq-default indent-tabs-mode nil)

;; Display the column number alongside the line number

(setq-default column-number-mode t)

;; I like the bar cursor

(setq-default cursor-type 'bar)

;; Use Visual Line Mode when editing text

(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; Show trailing whitespace in programming modes.

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; Use iBuffer instead of Buffer List.

(autoload 'ibuffer "ibuffer")
(global-set-key [remap list-buffers] 'ibuffer)

;; Hat Tip: https://www.emacswiki.org/emacs/UnwrapLine

(defun unwrap-line ()
  "Remove all newlines until we get to two consecutive ones.
    Or until we reach the end of the buffer.
    Great for unwrapping quotes before sending them via email."
  (interactive)
  (let ((start (point))
        (end (copy-marker (or (search-forward "\n\n" nil t)
                              (point-max))))
        (fill-column (point-max)))
    (fill-region start end)
    (goto-char start)))

;; Let Shift-Meta-Q do the opposite of Meta-Q

(global-set-key (kbd "M-Q") #'unwrap-line)

(provide 'init-misc)
