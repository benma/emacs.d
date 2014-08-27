(require 'hayoo)


(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(setenv "PATH" (concat "~/.cabal/bin:" (getenv "PATH")))
(add-to-list 'exec-path "~/.cabal/bin")

(load "haskell-mode-autoloads.el")
(add-hook 'haskell-mode-hook (lambda ()			       
;;			       (subword-mode +1)
			       (turn-on-haskell-doc-mode)
			       (turn-on-haskell-indentation)
			       ;;(structured-haskell-mode)
			       (ghc-init)
			       ))

(require 'company)
;;(add-hook 'haskell-mode-hook 'company-mode)
(add-to-list 'company-backends 'company-ghc)
(custom-set-variables '(company-ghc-show-info t))

(custom-set-variables
 '(haskell-tags-on-save t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-type 'cabal-repl))


(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-c h i") 'haskell-navigate-imports)
     (define-key haskell-mode-map (kbd "C-c h s") 'haskell-sort-imports)
     (define-key haskell-mode-map (kbd "C-c ,") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "C-c .") 'haskell-move-nested-right)

     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
     (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
     (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
     (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
     (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space))
     
     )

(eval-after-load 'haskell-cabal
  '(progn
     (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
     (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-ode-clear)
     (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
     (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))


(provide 'my-haskell)
