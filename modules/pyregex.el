;; pyregex
;; path to where the pyregex.py will be stored.
(setq pyregex-path (file-name-directory load-file-name)) ;; same directory as this file
(defun pyregex(regexp replace start end)
  "Regexp-replace using Python regexp and a Python expression for replace. Use \\number (1-based) or g[number] (0-based) for referencing groups and g for referencing the tuple of all groups.
If the supplied Python expression fails, it will be used as regexp replace string without evaluation.
Example usage 1:
regexp:
(\\d+)
replace:
(lambda n: n*2 if n<50 else n/2)(int(\\1)) OR
(lambda n: n*2 if n<50 else n/2)(int(g[0]))

Example usage 2, swap two chars and capitalize them:
regexp:
(.)(.)
replace:
(\"%s%s\" % (g[1], g[0])).upper() OR
(\"%s%s\" % (\\2, \\1)).upper() OR
''.join(reversed(g)).upper()
Note: g is the tuple of all groups.

Example usage 3, reverse all words
regexp:
\\b(.+?)\\b
replace:
\\1[::-1] OR
g[0][::-1]

Example usage 4, no Python expression evaluation
regexp:
abcd(.)(.)
replace:
abcd\\2\\1
Note: this is not a valid Python expression, so it will be used without evaluation.
"   
  (interactive (let (regexp replace)
		 ;;(unless (mark) (error "The mark is not set now, so there is no region"))
		 (setq regexp (read-from-minibuffer "Regexp? " ""))
		 (setq replace (read-from-minibuffer "Replace? " ""))
		 (list regexp
		       replace
		       (if (and transient-mark-mode mark-active) (region-beginning) (point))
		       (if (and transient-mark-mode mark-active) (region-end) (point-max)))))
  ;; create the code file to be executed if it doesn't exist yet
  (let ((pyregex-filename (expand-file-name "pyregex.py" pyregex-path)))
    (unless (file-exists-p pyregex-filename)
      (with-temp-file pyregex-filename
	(insert 
"import sys,re
regex,replace = sys.argv[1:]
input = sys.stdin.read()
try:
  sys.stdout.write(re.sub(regex,lambda m: (lambda g: str(eval(re.sub(r'\\\\(\\d+)', r'g[\\1-1]', replace))))(m.groups()),input))
except:
  sys.stdout.write(re.sub(regex,replace,input))
"
)))
    (let ((point-old (point)))
      (push-mark start)
      (goto-char end)
      (shell-command-on-region 
       start
       end
       (concat "python " (shell-quote-argument pyregex-filename) " " (shell-quote-argument regexp) " " (shell-quote-argument replace))
       t)
      (goto-char point-old)
      (pop-mark))
    )
  )

;; added automatically
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(provide 'pyregex)