(defvar ff/java-definition-keywords '("public" "private"))

(defun ff/function-follow (point-mark mark-region)
  "Find the definition of the selected/highlighted function"
  (interactive "r")
  (if (or (char-equal ?\( (char-after mark-region)) 
          (char-equal ?\( (char-after (1- mark-region))))
      (let ((regex 
             (ff/get-major-mode-keywords
              (buffer-substring-no-properties point-mark mark-region))))
        (if (or (re-search-backward regex nil t) (re-search-forward regex nil t))
            (beginning-of-line))))
  (deactivate-mark))

(defun ff/assemble-regex (function mode)
  "Assemble the regex for Java"
  (let ((rx "\\(") (list))
    (dolist (element ff/java-definition-keywords list)
      (setq rx (concat rx (mapconcat 'identity (cons element list) " ") "\\|")))
    (concat (substring rx 0 -2) "\\).*" (replace-regexp-in-string " " "" function))))

(defun ff/get-major-mode-keywords (function)
  "Get the keywords for the major mode
   to assemble the regex"
  (pcase major-mode
    (`java-mode    (ff/assemble-regex function ff/java-definition-keywords)) 
    (_             (message "Not a supported major mode"))))
