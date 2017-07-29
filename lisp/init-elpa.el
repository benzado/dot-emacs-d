;; Use MELPA to manage packages
;; M-x package-list-packages to browse and install

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             'append)

(setq package-enable-at-startup nil) ; don't autoload

(package-initialize)

(provide 'init-elpa)
