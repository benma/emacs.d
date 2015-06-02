
(menu-bar-mode -1) ;; no menu bar
(tool-bar-mode -1) ;; to tool bar
(setq inhibit-splash-screen t) ;; no welcome screen
(setq bookmark-save-flag 1) ;; save bookmarks on each set/delete
(setq ring-bell-function 'ignore) ;; no alarm bell
(setq scroll-step 1)
(defalias 'yes-or-no-p 'y-or-n-p) ;; short yes-or-now answers

;; various
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; no tabs, only spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Emacs will run garbage collection after `gc-cons-threshold' bytes of consing. The default value is 800,000 bytes, or ~ 0.7 MiB. By increasing to 10 MiB we reduce the number of pauses due to garbage collection.
;; Found here: http://www.wilfred.me.uk/.emacs.d/init.html#sec-3-21
(setq gc-cons-threshold (* 10 1024 1024))

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
(load-theme 'zenburn)
(load-theme 'zenburn-modeline-fix)
;;(set-face-attribute 'default nil :height 80) ;; 10pt instead of default 12pt.

(provide 'my-editor)
