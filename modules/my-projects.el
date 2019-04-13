;; projects

(require 'projectile)

;; workaround for non-projects
;; see https://github.com/bbatsov/projectile/issues/1270#issuecomment-423532882
(setq projectile-project-compilation-cmd "")

(add-to-list 'projectile-project-root-files "manage.py")
(setq projectile-svn-command projectile-generic-command)
(setq projectile-require-project-root nil)
;;(global-set-key "\C-c\c" 'projectile-compile-project)

(global-set-key
 "\C-c\c"
 (lambda ()
   "Calls a hydra based on the project name if it exists; or falls back to projectile-compile-project."
   (interactive)
   (let ((hydra-body (intern (format "hydra-compilation-%s/body" (projectile-project-name)))))
     (if (and (projectile-project-p) (commandp hydra-body))
         (call-interactively hydra-body)
           (call-interactively 'projectile-compile-project)))))


(define-key global-map [f6] 'projectile-find-file)

(defun project-compilation-cmd-noninteractive (command)
  "Runs a compilation in the project without prompting the user."
  (interactive)
  (let ((compilation-read-command nil))
    (projectile--run-project-cmd command nil
                                 :show-prompt nil
                                 :save-buffers t)))

(provide 'my-projects)
