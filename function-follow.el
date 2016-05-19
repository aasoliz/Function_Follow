(defun ff/function-follow (point-mark mark-region)
  "Find the definition of the selected function"
  (interactive "r")
  (if (or (char-equal ?\( (char-after mark-region)) 
          (char-equal ?\( (char-after (1- mark-region))))
          (let ((function (replace-regexp-in-string
                           " " "" (buffer-substring-no-properties point-mark mark-region))))
            (message "concat %s" (re-search-forward 
                                  (concat (concat "[(private)(static)(public)]* " function) "*")))))
  (deactivate-mark))
