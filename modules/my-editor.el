
(menu-bar-mode -1) ;; no menu bar
(tool-bar-mode -1) ;; to tool bar
(setq inhibit-splash-screen t) ;; no welcome screen
(setq bookmark-save-flag 1) ;; save bookmarks on each set/delete
(setq scroll-step 1)
(defalias 'yes-or-no-p 'y-or-n-p) ;; short yes-or-now answers

;; various
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; backups
(setq buffer-auto-save-file-name nil)
(setq make-backup-files nil) ;; do not make backup files (...~)
(setq auto-save-default nil) ;; do not make auto save files (#...#)
;; .#file files still created (lockfiles), maybe this will work
;; (setq create-lockfiles nil) --> disables refresh of externally modifed files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq tramp-backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq tramp-auto-save-directory temporary-file-directory)

;; theme
;; (when (or (window-system) (daemonp))
;;   (load-theme 'marko))
(load-theme 'zenburn)
(load-theme 'zenburn-modeline-fix)
(set-face-attribute 'default nil :height 80) ;; 10pt instead of default 12pt.

(provide 'my-editor)
