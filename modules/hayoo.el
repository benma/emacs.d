(require 'tabulated-list)
(require 'json)

(defvar hayoo--results)

(defun hayoo--query-and-list-results (query)
  ;; quick and dirty hayoo interface
  (url-retrieve
   (format "http://hayoo.fh-wedel.de/json/?query=%s" query)
   (lambda (status)
     (re-search-forward "\r?\n\r?\n")
     (let* ((res (json-read-from-string (buffer-substring (point) (point-max))))
            (results (cdr (assoc 'result res))))
       (setq hayoo--results results)
       (kill-buffer)
       (switch-to-buffer-other-window (hayoo--list-results))
       (message
        "Commands: [v] (visit package), [RET] (visit result); q to quit.")))
   nil t t))

(defun hayoo--current-result ()
  (elt hayoo--results (tabulated-list-get-id)))

(defun hayoo--visit-package ()
  (interactive)
  (browse-url (format "http://hackage.haskell.org/package/%s" (cdr (assoc 'resultPackage (hayoo--current-result))))))

(defun hayoo--visit-thing ()
  (interactive)
  (browse-url (cdr (assoc 'resultUri (hayoo--current-result)))))

(defvar hayoo-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "v" 'hayoo--visit-package)
    (define-key map "\C-m" 'hayoo--visit-thing)
    map)
  "Local keymap for `Hayoo' buffers.")

(define-derived-mode hayoo-mode tabulated-list-mode "Hayoo")

(defun hayoo--list-results ()
  (let ((buffer (get-buffer-create "*Hayoo*")))
    (with-current-buffer buffer
      (hayoo-mode)
      (setq tabulated-list-format
            (vector '("type" 10 t)
                    '("package" 13 t)
                    '("modules" 30 t)
                    '("name" 1 t)))
      ;; the entries below are populated sorted by score.
      ;; do not let tabulated-list-mode sort for us, s.t.
      ;; the orignal sort is preservedx
      (setq tabulated-list-sort-key nil)
      (let ((count (length hayoo--results))
            entries)
        
        (dotimes (i count)
         (let* ((result (elt results i))
                (result-type (cdr (assoc 'resultType result)))
                (result-name (cdr (assoc 'resultName result)))
                (result-package (cdr (assoc 'resultPackage result)))
                (result-modules (cdr (assoc 'resultModules result)))
                (result-uri (cdr (assoc 'resultUri result)))
                (result-description (cdr (assoc 'resultDescription result)))
                (result-signature (cdr (assoc 'resultSignature result)))
                (result-hackage (format "http://hackage.haskell.org/package/%s" result-package)))
           (push (list i (vector
                          result-type
                          
                          (cond ((equal result-type "package")
                                 result-name)
                                (t result-package))

                          (if result-modules
                                (elt result-modules 0)
                              "")

                          (if result-signature
                              (format "%s :: %s" result-name result-signature)
                            result-name)))
                 entries)))
        (setq tabulated-list-entries (nreverse entries)))
      (tabulated-list-init-header)
      (tabulated-list-print)
      )
    buffer))

(defun hayoo-query (query)
  "Do a Hayoo search for QUERY."
  (interactive
   (let ((def (haskell-ident-at-point)))
     (if (and def (symbolp def)) (setq def (symbol-name def)))
     (list (read-string (if def
                            (format "Hayoo query (default %s): " def)
                          "Hayoo query: ")
                        nil nil def))))
  (hayoo--query-and-list-results query))

(provide 'hayoo)
