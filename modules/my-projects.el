;; projects

(require 'projectile)

(add-to-list 'projectile-project-root-files "manage.py")
(setq projectile-svn-command projectile-generic-command)
(setq projectile-require-project-root nil)
(global-set-key "\C-c\c" 'projectile-compile-project)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(define-key global-map [f6] 'projectile-find-file)

(provide 'my-projects)
