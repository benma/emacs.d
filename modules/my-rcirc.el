(require 'rcirc)

(require 'rcirc-notify)
(rcirc-notify-add-hooks)
(setq my-rcirc--keywords (list "goats!"))

(require 'rcirc-color)
(setq rcirc-color-is-deterministic t)

(require 'rcirc-styles)

;; Smaller server messages.
(set-face-attribute 'rcirc-server nil :foreground "Firebrick" :height 0.6)

(add-hook 'rcirc-mode-hook
          (lambda ()
            (auto-complete-mode -1)))

(defadvice rcirc (before rcirc-authinfo activate)
  (setq rcirc-server-alist
        `(("irc.freenode.net" :port 6697 :encryption tls
           :nick "benma" :user-name "benma"
           :channels ("#haskell" "#emacs"))
          ("irc.monetas.io" :port 6697 :encryption tls
           :nick "benma" :user-name "mbencun"
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

(defun my-rcirc-format ()
  (interactive)
  (let* ((choices (union
                   rcirc-styles-attribute-alist
                   '(("\C-c0" . white)
                     ("\C-c1" . black)
                     ("\C-c2" . blue)
                     ("\C-c3" . green)
                     ("\C-c4" . red)
                     ("\C-c5" . brown)
                     ("\C-c6" . purple)
                     ("\C-c7" . orange)
                     ("\C-c8" . yellow)
                     ("\C-c9" . lightgreen)
                     ("\C-c10" . cyan)
                     ("\C-c11" . lightcyan)
                     ("\C-c12" . lightblue)
                     ("\C-c13" . pink)
                     ("\C-c14" . gray)
                     ("\C-c15" . lightgray)
                   )))
         (default 'bold)
         (choice (intern (completing-read "format: " (mapcar 'symbol-name (mapcar 'cdr choices)) nil t nil nil default))))
    (insert-string (car (rassoc choice choices)))))

(provide 'my-rcirc)
