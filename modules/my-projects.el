;; projects 
;; (require 'project-root)
;; (setq project-roots
;;       '(("UMS"
;;          :root-contains-files ("ums.wsgi")
;;          )))

(require 'projectile)
(add-to-list 'projectile-project-root-files "manage.py")
(setq projectile-svn-command projectile-generic-command)

(define-key global-map [f6] 'projectile-find-file)

(provide 'my-projects)
