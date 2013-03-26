;; projects 
;; (require 'project-root)
;; (setq project-roots
;;       '(("UMS"
;;          :root-contains-files ("ums.wsgi")
;;          )))

(require 'projectile)
(add-to-list 'projectile-project-root-files "manage.py")
(setq projectile-generic-command "find . ! -name '*.pyc' ! -name '*.mo' -type f -print0")
(setq projectile-svn-command projectile-generic-command)

(defun my-projectile-hashify-path-suffix (files-list)
  "Display files as filename|relative_path"
  (let ((project-root (projectile-project-root))
        (files-table (make-hash-table :test 'equal)))
    (dolist (current-file files-list files-table)
      (let ((key (file-relative-name current-file project-root)))
	(setq key (replace-regexp-in-string "\\(.*?\\)\\([^/]+?\\)$" "\\2|\\1" key))
	;; remove trailing | or /
	(setq key (replace-regexp-in-string "\\(|\\|/\\)$" "" key))
      (puthash key current-file files-table)))))
(setq projectile-show-paths-function 'my-projectile-hashify-path-suffix)
(define-key global-map [f6] 'projectile-find-file)

(provide 'my-projects)
