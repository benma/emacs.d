(require 'package)
(require 'melpa)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(setq url-http-attempt-keepalives nil)

(defvar my-packages
  '(expand-region ace-jump-mode haskell-mode python linum auto-complete yasnippet undo-tree dired+ ack-and-a-half deft volatile-highlights magit gnuplot)
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
