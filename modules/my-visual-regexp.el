
(add-to-list 'load-path (concat my-modules-dir "visual-regexp/"))
(add-to-list 'load-path (concat my-modules-dir "visual-regexp-steroids/"))
(require 'visual-regexp-steroids)

(defmacro vr-prefix-engine-selector (func)
  `(lambda (arg)
     (interactive "P")
     (let ((vr/engine (if arg 'python 'emacs)))
       (call-interactively ,func))))

(defun vr-prefixed-mc-mark (arg) ;; choose visual regexp engine with prefix arg
  (interactive "P")
  (let ((vr/engine (if arg 'python 'emacs)))
    (call-interactively 'vr/mc-mark)))

(add-hook 'multiple-cursors-mode-enabled-hook
	  ;; run vr/mc-mark once per cursor by default (do not ask the user)
	  (lambda ()
	    (when (boundp 'mc--default-cmds-to-run-once)
	      (add-to-list 'mc--default-cmds-to-run-once 'vr-prefixed-mc-mark))))

(define-key global-map (kbd "C-c r") (vr-prefix-engine-selector 'vr/replace))
(define-key global-map (kbd "C-c q") (vr-prefix-engine-selector 'vr/query-replace))
(define-key global-map (kbd "C-c m") 'vr-prefixed-mc-mark)

(define-key esc-map (kbd "C-r") 'vr/isearch-backward)
(define-key esc-map (kbd "C-s") 'vr/isearch-forward)

(provide 'my-visual-regexp)
