(require 'rcirc)

(require 'rcirc-notify)
(rcirc-notify-add-hooks)

(require 'rcirc-color)
(setq rcirc-color-is-deterministic t)

(add-hook 'rcirc-mode-hook (lambda ()
                             (auto-complete-mode -1)))


(setq rcirc-server-alist
      `(("irc.freenode.net" :port 6697 :encryption tls
         :nick "benma" :user-name "benma"
         :channels ("#haskell" "#emacs"))
        ("irc.monetas.io" :port 6697 :encryption tls
         :nick "benma" :user-name "marko"
         :password ,(secrets-get-secret "Login" "monetas ldap")
         :channels ("#monetas-dev" "#monetas"))
        ))

;; Set account info from gnome-keyring.
(setq rcirc-authinfo `(("irc.freenode.net" nickserv
                       ,"benma"
                       ,(secrets-get-secret "Login" "IRC Freenode"))))

(provide 'my-rcirc)
