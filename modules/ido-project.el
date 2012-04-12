

(defun ido-project-files ()
  "Use ido to select a file from the project."
  (interactive)
  (let (my-project-root project-files tbl)
  (unless project-details (project-root-fetch))
  (setq my-project-root (cdr project-details))

  ;; get project files
  (setq project-files 
	(split-string 
	 (shell-command-to-string 
	  (concat "find "
		  my-project-root
		  " \\( -name \"*.svn\" -o -name \"*.git\" \\) -prune -o -type f -print | grep -E -v \"\.(pyc)$\""
		  )) "\n"))
  ;; populate hash table (display repr => path)
  (setq tbl (make-hash-table :test 'equal))
  (let (ido-list)
  (mapc (lambda (path)
	  
	  ;; format path for display in ido list
	  (setq key (replace-regexp-in-string "\\(.*?\\)\\([^/]+?\\)$" "\\2|\\1" path))
	  ;; strip project root
	  (setq key (replace-regexp-in-string my-project-root "" key))
	  ;; remove trailing | or /
	  (setq key (replace-regexp-in-string "\\(|\\|/\\)$" "" key))
	  (puthash key path tbl)
	  (push key ido-list)
	  )
	project-files
	)
  (find-file (gethash (ido-completing-read "project-files: " ido-list) tbl)))))

(provide 'ido-project)