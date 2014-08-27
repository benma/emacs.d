(require 'w3m)
(require 'json)

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


(defun my-hayoo-show (query)
  ;; quick and dirty hayoo interface
  (url-retrieve
   (format "http://hayoo.fh-wedel.de/json/?query=%s" query)
   (lambda (status)
     (message "%s" status)
     (re-search-forward "\r?\n\r?\n")
     (let* ((res (json-read-from-string (buffer-substring (point) (point-max))))
            (count (cdr (assoc 'count res)))
            (results (cdr (assoc 'result res))))
       (erase-buffer)
       (dotimes (i count)
         (let* ((result (elt results i))
                (result-type (cdr (assoc 'resultType result)))
                (result-name (cdr (assoc 'resultName result)))
                (result-package (cdr (assoc 'resultPackage result)))
                (result-uri (cdr (assoc 'resultUri result)))
                (result-description (cdr (assoc 'resultDescription result)))
                (result-signature (cdr (assoc 'resultSignature result)))
                (result-hackage (format "http://hackage.haskell.org/package/%s" result-package)))
           (cond ((equal result-type "function")
                  (insert (format "<a href=\"%s/../../\">%s</a>/" result-uri result-package))
                  (insert (format "<a href=\"%s\">%s</a> :: %s<br/>" result-uri result-name result-signature))
                  (insert result-description)
                  (insert "<br/><br/>"))))))

     ;; display the above html in w3m
     (let ((browse-url-browser-function 'w3m-browse-url))
       (browse-url-of-buffer)))
   nil t t))

;;;###autoload
(defun my-hayoo (query)
  "Do a Hayoo search for QUERY."
  (interactive
   (let ((def (haskell-ident-at-point)))
     (if (and def (symbolp def)) (setq def (symbol-name def)))
     (list (read-string (if def
                            (format "Hayoo query (default %s): " def)
                          "Hayoo query: ")
                        nil nil def))))
  (my-hayoo-show query))

(provide 'my-haskell)

(my-hayoo "callTemplate")
