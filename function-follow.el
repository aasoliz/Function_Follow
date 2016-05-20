(defvar ff/java-definition-keywords     '("public" "private"))
(defvar ff/lisp-definition-keywords     '("defun"))
(defvar ff/python-definition-keywords   '("def"))
(defvar ff/perl-definition-keywords     '("sub"))

(defun ff/function-follow (point-mark mark-region)
  "Find the definition of the selected/highlighted function"
  (interactive "r")
  (deactivate-mark)
  (block follow
    (if (or (char-equal ?\( (char-after mark-region)) 
            (char-equal ?\( (char-after (1- mark-region)))
            (char-equal ?\( (char-before point-mark)))
        (let (position window (regex 
                               (ff/get-major-mode-keywords
                                (buffer-substring-no-properties point-mark mark-region))))
          (if (or (re-search-backward regex nil t) (re-search-forward regex nil t))
              (progn
                (beginning-of-line)
                (return-from follow)))
          (dolist (element (ff/search-open-buffers (substring (symbol-name major-mode) 0 -5)) nil)
            (set-buffer element)
            (if (setq position (re-search-forward regex nil t))
                (progn
                  (ff/displaying-buffer element position)
                  (return-from follow))
              (if (not (eq (setq window (get-buffer-window element)) nil))
                  (if (setq position (re-search-backward regex nil t))
                      (progn
                        (ff/displaying-buffer-with-window position window)
                        (return-from follow))))))
          (message "Could not find the function"))
      (message "Did not detect method call"))))

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
