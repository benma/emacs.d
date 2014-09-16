(setq my-root-dir (file-name-directory user-init-file))
(setq my-modules-dir (concat my-root-dir "modules/"))
(setq my-vendor-dir (concat my-root-dir "vendor/"))
(setq my-snippets-dir (concat my-root-dir "snippets/"))

(add-to-list 'load-path my-modules-dir)
(add-to-list 'load-path my-vendor-dir)
(add-to-list 'custom-theme-load-path (concat my-root-dir "themes/"))

(require 'cl)

(setq debug-on-error t)
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
;; (add-to-list 'load-path (concat my-vendor-dir "magit/"))
(global-set-key "\C-c\g" 'magit-status)
;; full screen magit-status
(require 'magit)
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (magit-mode-quit-window)
  (jump-to-register :magit-fullscreen))
(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

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

;; ack
(require 'ack-and-a-half)
(setq ack-and-a-half-prompt-for-directory t)
(global-set-key (kbd "<f8>") 'ack-and-a-half)

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

;; CG
(require 'cg-mode)

;; dired+
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
(require 'auto-complete-config)
(setq ac-auto-show-menu 0.0)
(global-auto-complete-mode t)
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
			   (auto-complete-mode 1))
		       ))
(real-global-auto-complete-mode t)

;; erc
(add-hook 'erc-mode-hook (lambda ()
                           (auto-complete-mode -1)))
(setq erc-nick "benma")
;; find password in ~/.authinfo
(setq erc-prompt-for-password nil)
;; enable channel autojoin
(require 'erc-join)
(erc-autojoin-enable)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#haskell" "#snapframework")))

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

;; interactive buffers/files
(ido-mode)
(ido-everywhere 1)
 (flx-ido-mode 1)
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
(global-undo-tree-mode)

(require 'back-button)
(back-button-mode 1)

(require 'my-visual-regexp)

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

(defun visit-ansi-term ()
  "If the current buffer is:
     1) an ansi-term, create a new one
     2) not an ansi-term, switch to *ansi-term* if it exists, otherwise create new one."
  (interactive)
  (let ((is-term (string= "term-mode" major-mode))
        (term-cmd "/bin/bash"))
    (if is-term
        (ansi-term term-cmd)
      (if (get-buffer "*ansi-term*")
          (if (term-check-proc "*ansi-term*")
              (switch-to-buffer "*ansi-term*")
            (ansi-term term-cmd))
        (ansi-term term-cmd))))
  (term-set-escape-char ?\C-c))
(global-set-key (kbd "<f4>") 'visit-ansi-term)
;; (define-key global-map [f4] (lambda ()
;;   (interactive nil)
;;   (ansi-term "/bin/bash")
;;   ;;(term-line-mode)
;; ))


(global-set-key "\M-k" 'kill-whole-line)

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
