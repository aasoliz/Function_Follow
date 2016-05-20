(defvar ff/java-definition-keywords     '("public" "private"))
(defvar ff/lisp-definition-keywords     '("defun"))
(defvar ff/python-definition-keywords   '("def"))
(defvar ff/perl-definition-keywords     '("sub"))

(defun ff/function-follow (point-mark mark-region)
  "Find the definition of the selected/highlighted function"
  (interactive "r")
  (if (or (char-equal ?\( (char-after mark-region)) 
          (char-equal ?\( (char-after (1- mark-region)))
          (char-equal ?\( (char-before point-mark)))
      (let ((regex 
             (ff/get-major-mode-keywords
              (buffer-substring-no-properties point-mark mark-region))))
        (if (or (re-search-backward regex nil t) (re-search-forward regex nil t))
            (beginning-of-line)
          (message "Couldn't find the function")))
    (message "Did not detect method call"))
  (deactivate-mark))

(defun ff/assemble-regex (function mode &optional stop)
  "Assemble the regex to find the function definition"
  (let ((rx "\\(") (list))
    (dolist (element mode list)
      (setq rx 
            (concat rx (mapconcat 'identity (cons element list) " ") "\\|")))
    (if (string= stop "perl")
        (concat (substring rx 0 -2) "\\).* " (replace-regexp-in-string " " "" function) ".*{")
      (concat (substring rx 0 -2) "\\).* " (replace-regexp-in-string " " "" function) ".*("))))

(defun ff/get-major-mode-keywords (function)
  "Get the keywords for the major mode
   to assemble the regex inorder to find
   the function definition"
  (pcase major-mode
    (`java-mode        (ff/assemble-regex function ff/java-definition-keywords)) 
    (`emacs-lisp-mode  (ff/assemble-regex function ff/lisp-definition-keywords))  
    (`python-mode      (ff/assemble-regex function ff/python-definition-keywords))
    (`perl-mode        (ff/assemble-regex function ff/perl-definition-keywords "perl"))
    (`ruby-mode        (ff/assemble-regex function ff/python-definition-keywords))
    (_                 (message "Not a supported major mode"))))
