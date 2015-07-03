(require 'rcirc)

(require 'rcirc-notify)
(rcirc-notify-add-hooks)

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


(provide 'my-rcirc)
