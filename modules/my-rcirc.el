(require 'rcirc)

(require 'rcirc-notify)
(rcirc-notify-add-hooks)
(setq my-rcirc--keywords (list "goats!"))

(require 'rcirc-color)
(setq rcirc-color-is-deterministic t)

(add-hook 'rcirc-mode-hook
          (lambda ()
            (auto-complete-mode -1)))

(defadvice rcirc (before rcirc-authinfo activate)
  (setq rcirc-server-alist
        `(("irc.freenode.net" :port 6697 :encryption tls
           :nick "benma" :user-name "benma"
           :channels ("#haskell" "#emacs"))
          ("irc.monetas.io" :port 6697 :encryption tls
           :nick "benma" :user-name "marko"
             ;; Get password from gnome-keyring.
           :password ,(secrets-get-secret "Login" "monetas ldap")
           :channels ("#monetas-dev" "#monetas"))
          ))
  (setq rcirc-authinfo `(("irc.freenode.net" nickserv
                          ,"benma"
                          ;; Get password from gnome-keyring.
                          ,(secrets-get-secret "Login" "IRC Freenode")))))


(defun my-rcirc--notify-keywords (proc sender response target text)
  "Notify the current user when someone sends a message that
matches a keyword."
  (interactive)
  (when (and (not (string= (rcirc-nick proc) sender))
             (not (string= (rcirc-server-name proc) sender))
             (rcirc-channel-p target)
             (rcirc-notify-allowed sender))
    (let ((keyword (catch 'match
                     (dolist (key my-rcirc--keywords)
                       (when (string-match key text)
                         (throw 'match key))))))
      (when keyword
        (rcirc-notify-keyword sender keyword text)))))

(add-hook 'rcirc-print-hooks 'my-rcirc--notify-keywords)

(provide 'my-rcirc)
