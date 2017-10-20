(require 'package)
;; (require 'melpa)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)
(setq url-http-attempt-keepalives nil)

(defvar my-packages
  '(hayoo
    visual-regexp
    visual-regexp-steroids
    sane-term
    go-mode
    go-autocomplete
    go-dlv
    fill-column-indicator
    expand-region
    ggtags
    ace-jump-mode
    haskell-mode ghc shm company-ghc
    key-chord
    zenburn-theme
    evil
    linum
    auto-complete
    yasnippet
    undo-tree
    dired+
    markdown-mode
    ag
    deft
    volatile-highlights
    magit
    git-link
    gnuplot
    smex
    back-button
    xclip
    multiple-cursors
    projectile
    flx-ido
    grep-a-lot
    cmake-mode
    tern
    tern-auto-complete
    rcirc-color
    rcirc-styles
    rcirc-notify
    browse-at-remote)
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs  is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))



(provide 'my-packages)
