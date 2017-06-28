;; Send mail using a SMTP, because I don't use Emacs on a mail server.

(setq send-mail-function #'smtpmail-send-it)

;; It's me!

(setq user-full-name "Benjamin Ragheb"
      user-mail-address "ben@benzado.com")

;; Use FastMail's SMTP server. The login and password are stored in
;; ~/.authinfo.gpg (encrypted, because I'm no dummy).

(setq smtpmail-smtp-server "smtp.fastmail.com")

;; The port we connect to depends on whether Emacs can do TLS directly;
;; see https://www.fastmail.com/help/technical/servernamesandports.html

(if (gnutls-available-p)
    (setq smtpmail-smtp-service 465
          smtpmail-stream-type 'ssl)
  (setq smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls))

(provide 'init-sendmail)
