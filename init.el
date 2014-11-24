(setq my-root-dir (file-name-directory user-init-file))
(setq my-modules-dir (concat my-root-dir "modules/"))
(setq my-vendor-dir (concat my-root-dir "vendor/"))
(setq my-snippets-dir (concat my-root-dir "snippets/"))

(add-to-list 'load-path my-modules-dir)
(add-to-list 'load-path my-vendor-dir)

(require 'my-packages)

;; remove clutter
(menu-bar-mode -1) ;; no menu bar
(tool-bar-mode -1) ;; to tool bar
(setq inhibit-splash-screen t) ;; no welcome screen

;; evil
(require 'evil)
(evil-mode 1)
