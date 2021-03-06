
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq my-root-dir (file-name-directory user-init-file))
(setq my-modules-dir (concat my-root-dir "modules/"))
(setq my-vendor-dir (concat my-root-dir "vendor/"))
(setq my-snippets-dir (concat my-root-dir "snippets/"))

(add-to-list 'load-path my-modules-dir)
(add-to-list 'load-path my-vendor-dir)
(add-to-list 'custom-theme-load-path (concat my-root-dir "themes/"))

(setenv "PATH" (concat "~/bin" (getenv "PATH")))
(add-to-list 'exec-path (getenv "PATH"))


(require 'cl)

;; config changes made through the customize UI will be stored here
(setq custom-file (concat my-root-dir "custom.el"))
;; load this file
(when (file-exists-p custom-file) (load custom-file))

(require 'my-packages)
(require 'my-editor)

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.cu$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;;;; Modes

;; fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "red")

;; ggtags (gnu global)
(eval-after-load 'ggtags
  (setq ggtags-enable-navigation-keys nil))
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

;; c, c++
(require 'cc-mode)
(c-add-style "my-c-style"
	     '("gnu"
	       (indent-tabs-mode . nil)        ; use spaces rather than tabs
	       (c-basic-offset . 4)            ; indent by four spaces
	       (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                               (brace-list-open . 0)
                               (arglist-intro . 4)
				   (statement-case-open . +)))))
(add-hook 'c++-mode-hook (lambda ()
                         (c-set-style "my-c-style")
                         ;; show col 80 visually
                         (setq fill-column 100)
                         (fci-mode)))
(add-hook 'c-mode-hook (lambda ()
                         (c-set-style "my-c-style")
                         ;; show col 80 visually
                         (setq fill-column 100)
                         (fci-mode)))

(require 'clang-format)
(global-set-key (kbd "C-c f") 'clang-format-region)

;; js
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))

;; w3m

(eval-after-load 'w3m
  '(progn
     ;; open link in external browser
     (define-key w3m-mode-map "F" 'w3m-view-url-with-browse-url)))

;; xclip

(require 'xclip) ;; requires xclip command line tool
(xclip-mode t)

;; po-mode
;; Do not replace my header only because PO-Revision-Date is missing!
(defadvice po-check-file-header (around no-po-check-file-header activate)
  ;; do nothing
  )

;;(require 'my-latex)

;; magit
(require 'my-magit)
;; gnuplot
(add-to-list 'auto-mode-alist '("\\.gnu$" . gnuplot-mode))
(add-hook 'gnuplot-mode-hook (lambda ()
			       (unless (file-exists-p "Makefile")
				 (set (make-local-variable 'compile-command)
				      (concat "gnuplot " buffer-file-name)))))

;; (electric-pair-mode t)
;; (electric-indent-mode t)
;; (electric-layout-mode t)y
(show-paren-mode t)
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; ag
(require 'ag)
(global-set-key (kbd "<f8>") 'ag-regexp)

(require 'grep-a-lot)
(grep-a-lot-setup-keys)
(require 'ack-and-a-half-a-lot)

;; we don't want to start in root directoy by default,
;; but in directory of current file
;; todo: make two bindings, one with default directory=root directory and
;; one with default directory=current directory.
(setq ack-and-a-half-root-directory-functions nil)
;; skip migrations folders in django projects
(setq ack-and-a-half-arguments '("--ignore-dir migrations"))
(global-set-key (kbd "<f9>") 'next-error)
(global-set-key (kbd "S-<f9>") 'previous-error)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "<f7>") 'er/expand-region)

;; go-playground
(eval-after-load 'go-playground
  '(define-key go-playground-mode-map (kbd "C-c c") 'go-playground-exec))

;; ace-jump-mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(setq ace-jump-mode-scope 'window)


;; html-mode
(add-hook 'html-mode-hook (lambda ()
			    (setq tab-width 4) ;; tab width
			    ))

;; haskell-mode
(require 'my-haskell)

;; go-mode
(require 'go-mode)
(setq gofmt-command "goimports")
(setq gofmt-is-goimports t)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
                          ;; show col 100 visually
                          (setq fill-column 100)
                          (fci-mode)))

;; company-mode
(setq company-idle-delay 0)
(setq company-show-numbers t)
(add-hook 'after-init-hook 'global-company-mode)
;; (require 'company-tabnine)
;; (add-to-list 'company-backends #'company-tabnine)


;; workaround for distorted popup when fci-mode is enabled:
;; https://github.com/company-mode/company-mode/issues/180#issuecomment-55047120
(defvar-local company-fci-mode-on-p nil)
(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))
(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))
(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)


;; CG
(require 'cg-mode)

;; dired+
;; https://emacs.stackexchange.com/questions/33842/dired-hangs-emacs-in-no-window-mode
(setq diredp-bind-problematic-terminal-keys nil)
(require 'dired+)

;; dired
(put 'dired-find-alternate-file 'disabled nil)
;; don't open new buffer when opening directory
(add-hook 'dired-mode-hook (lambda ()
			     (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
			     (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))
			     ;; change to wdired mode: rename files directly in the buffer.
			     (define-key dired-mode-map (kbd "C-c e") (lambda () (interactive) (wdired-change-to-wdired-mode)))
			     ))


;; python
(require 'python)
(add-hook 'python-mode-hook (lambda ()
			      ;; "_" is a delimiter in words (when moving C-left/right
			      ;; (modify-syntax-entry ?_ "_" py-mode-syntax-table)

			      ;; disable electric indent mode, it indents the wrong line in python mode
			      (electric-indent-mode -1)))

;; ;; linum
;; (require 'linum)
;; (global-linum-mode t)
;; (setq linum-format "%2d ")

;; uniquify
(require 'uniquify) ;; uniquifies identically-named buffers by appending part of the path.
(setq uniquify-buffer-name-style 'post-forward)

;; windmove
;; shift-up is broken with xterm-256color?
;; rebuild xterm-256color will fix it (http://forums.vandyke.com/showpost.php?p=26580&postcount=25):
;; (maybe it is also in this folder: /usr/share/terminfo/x/)
;; sudo cp /lib/terminfo/x/xterm-256color /lib/terminfo/x/xterm-256color.backup
;; infocmp xterm-256color > xterm.tmp
;; tic xterm.tmp
;; sudo cp ~/.terminfo/x/xterm-256color /lib/terminfo/x/xterm-256color
;; rm xterm.tmp
(windmove-default-keybindings) ;; shift + arrowkeys
(setq windmove-wrap-around t)

;; in terminal, when TERM=screen-256color, shift-(left|right|...) etc. do not work. bind them explicitely:
(defadvice terminal-init-screen(around map-S-escape-sequences activate)
  ;; defadvice needed so that it also works with emacsclient.
  (define-key input-decode-map "\e[1;2D" [S-left])
  (define-key input-decode-map "\e[1;2C" [S-right])
  (define-key input-decode-map "\e[1;2B" [S-down])
  (define-key input-decode-map "\e[1;2A" [S-up])
  (define-key input-decode-map "\e[1;2F" [S-end])
  (define-key input-decode-map "\e[1;2H" [S-home])
  (define-key input-decode-map "\e[1;5D" [C-left])
  (define-key input-decode-map "\e[1;5C" [C-right])
  (define-key input-decode-map "\e[1;5B" [C-down])
  (define-key input-decode-map "\e[1;5A" [C-up])
  ad-do-it
  )
;; same for some keys when TERM=xterm-256color
(defadvice terminal-init-xterm(around map-S-escape-sequences activate)
  ;; defadvice needed so that it also works with emacsclient.
  (define-key input-decode-map "\e[4~" [end])
  ad-do-it
  )

;; execute commands by hitting two keys simultaneously.
(require 'key-chord)
;; reduce delay times s.t. you don't accidentally trigger a key-chord
;; during normal typing.
(setq key-chord-two-keys-delay .015
      key-chord-one-key-delay .020)
(key-chord-mode 1)
(key-chord-define-global "§1" 'keyboard-escape-quit)
(key-chord-define-global "j1" 'delete-other-windows)
(key-chord-define-global "j2" 'split-window-vertically)
(key-chord-define-global "j3" 'split-window-horizontally)
(key-chord-define-global "j0" 'delete-window)
(key-chord-define-global "fb" 'ido-switch-buffer)
;;(key-chord-define-global "/s" 'save-buffer)
(key-chord-define-global "fv" 'ido-find-file)
(key-chord-define-global "z," 'beginning-of-buffer)
(key-chord-define-global "z." 'end-of-buffer)

;; yasnippet
(add-hook 'term-mode-hook (lambda()
			    (yas-minor-mode -1)))
(require 'yasnippet)
(add-to-list 'yas/snippet-dirs my-snippets-dir)
(yas/global-mode 1)

;; auto-complete
;; (require 'auto-complete-config)
;; (setq ac-auto-show-menu 0.0)
;; (global-auto-complete-mode t)

;; define sources
;; ac-source-yasnippet is broken. until it is fixed, use simple configuration
(setq-default ac-sources '(ac-source-dictionary ac-source-words-in-same-mode-buffers))
;; (setq-default ac-sources '(ac-source-dictionary ac-source-words-in-same-mode-buffers ac-source-yasnippet))
;; (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;; (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
;; (add-hook 'css-mode-hook 'ac-css-mode-setup)
;; (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                           (auto-complete-mode 1))))
;;(real-global-auto-complete-mode t)

;; erc
(require 'erc)
(add-hook 'erc-mode-hook (lambda ()
                           (auto-complete-mode -1)))
(setq erc-nick "benma")
(setq erc-user-full-name "Marko Bencun")

;; find password in ~/.authinfo
(setq erc-prompt-for-password nil)
;; logging
(setq erc-log-channels-directory "~/.erc/logs/")
(setq erc-save-buffer-on-part t)
;; notification
(add-to-list 'erc-modules 'notifications)
;; enable channel autojoin
(require 'erc-join)
(erc-autojoin-enable)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#monetas-dev" "#haskell")))

;; rcirc
(require 'my-rcirc)

;; rectangle editing
(require 'rect-mark)
;; (global-set-key (kbd "C-t") 'rm-set-mark)
(global-set-key (kbd "C-x r C-w") 'rm-kill-region)
(global-set-key (kbd "C-x r M-w") 'rm-kill-ring-save)

;; ;; cua-mode only for rectangle editing
;; (setq cua-enable-cua-keys nil) ;; ctrl-enter -> rectangle editing,
;; ;;(setq cua-highlight-region-shift-only t) ;; no transient mark mode
;; ;;(setq cua-toggle-set-mark nil) ;; original set-mark behavior, i.e. no transient-mark-mode
;; (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
;; ;;(setq cua-delete-selection nil)
;; (global-set-key "\C-t" 'cua-set-rectangle-mark)
;; (cua-mode t)
(delete-selection-mode t)

;; winner-mode
;; Use C-c <left> to restore previous frame configuration (e.g, restore after maximizing a buffer).
(winner-mode 1)

;; projects
(require 'my-projects)

;; hydra
(require 'my-hydra)

;; interactive buffers/files
(ido-mode)
(ido-everywhere 1)
;;(flx-ido-mode 1)

;; go straight home by pressing ~
(add-hook 'ido-setup-hook (lambda ()
			    (define-key ido-file-completion-map
			      (kbd "~")
			      (lambda ()
				(interactive)
				(if (looking-back "/")
				    (insert "~/")
				  (call-interactively 'self-insert-command))))))


;; smex (M-x with ido)
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-x M-x") 'execute-extended-command)

;; undo-tree (use C-x u to visualize, C-_ to undo, M-_ to redo)
(require 'undo-tree)
(setq undo-tree-enable-undo-in-region nil)
(global-undo-tree-mode)

(require 'back-button)
(back-button-mode 1)

(require 'my-visual-regexp)

;; browse-at-remote
(require 'browse-at-remote)
(add-to-list 'browse-at-remote-remote-type-domains '("bitbucket.monetas.io" ."monetas"))
(defun browse-at-remote--format-region-url-as-monetas (repo-url location filename &optional linestart _lineend)
  "URL formatted for bitbucket"
  (let* ((repo (if (string= (substring repo-url -3) "got") "gotary" "jarlie"))
        (revision (vc-git-working-revision (buffer-file-name)))
        (url (format "https://bitbucket.monetas.io/projects/GOT/repos/%s/browse/%s?at=%s" repo filename revision)))
    (if linestart (format "%s#%d" url linestart) url)))

;;;; Keybindings

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "M-S-<up>") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-S-<down>") 'mc/mark-next-like-this)

;; use C-y in search to yank last killed text into the minibuffer
(add-hook 'isearch-mode-hook
	  (lambda ()
	    (define-key isearch-mode-map (kbd "C-e") 'isearch-edit-string)
	    (define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)))

;; use clipboard too
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

(define-key global-map [f3] 'query-replace)

;; RET = newline-and-indent in programming modes
;; (electric-indent-mode t) ;; weird, moves current line to right in python-mode...
(add-hook 'prog-mode-hook '(lambda ()
			     (local-set-key (kbd "RET") 'newline-and-indent)))

(require 'term)
;; kill term buffer on exit
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)


(require 'sane-term)
(defun my-sane-term (arg)
  (interactive "P")
  (if arg
      (sane-term-create)
    (sane-term)))
(global-set-key (kbd "<f4>") 'my-sane-term)

;; misc
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-set-key "\M-k" 'kill-whole-line)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(defun whack-whitespace ()
  "Delete all white space from point to the next word."
  (interactive nil)
  (when (re-search-forward "[ \t\n]+" nil t)
    (replace-match "" nil nil)))
(global-set-key "\C-l" 'whack-whitespace)

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "<C-return>") 'open-line-below)

(defun django-pdb ()
  (interactive)
  (let ((manage-file (concat (projectile-project-root) "manage.py")))
    (if (file-exists-p manage-file)
	(pdb (concat manage-file " runserver --noreload"))
      (error "You are not in a Django project"))))

;; LSP
;; (require 'lsp-mode)
;; (add-hook 'go-mode-hook #'lsp-deferred)
;; (add-hook 'rust-mode-hook #'lsp-deferred)
;; (require 'company-lsp)
;; (push 'company-lsp company-backends)
