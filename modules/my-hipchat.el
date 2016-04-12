(require 'jabber-hipchat)
(setq hipchat-nickname "Marko Bencun"
      hipchat-muc-domain "conf.btf.hipchat.com"
      hipchat-api-url "https://hipchat.monetas.io/v2"
      hipchat-api-key (secrets-get-secret "Login" "hipchat api key")
      hipchat-autojoin '("1_sysops@conf.btf.hipchat.com" "1_privacy_ideas@conf.btf.hipchat.com" "1_monetas@conf.btf.hipchat.com" "1_las_palmas@conf.btf.hipchat.com" "1_android@conf.btf.hipchat.com" "1_devs@conf.btf.hipchat.com" "1_gotary@conf.btf.hipchat.com" "1_watercooler@conf.btf.hipchat.com")
      jabber-mode-line-mode t
      jabber-muc-colorize-foreign t
      jabber-otr-message-history t
      jabber-account-list `(("1_14@chat.btf.hipchat.com"
                             (:password . ,(secrets-get-secret "Login" "monetas ldap"))
                             (:network-server . "hipchat.monetas.io")
                             (:port . 5223)
                             (:connection-type . ssl))))

(add-hook 'jabber-post-connect-hooks 'hipchat-setup-hook)

(add-hook 'jabber-chat-mode-hook
          (lambda ()
            (auto-complete-mode -1)))

(provide 'my-hipchat)
