(defun ff/search-files (extension)
  "Search through other files in the directory
   looking for a match. Only check files with
   the same extension."
  (let (list index)
    (dolist (element (directory-files "." nil nil t) list)
      (if (and (not (eq (setq index (search "." element)) nil))
               (string= (substring element (1+ index)) extension)
               (not (ff/buffer-exists element))
               (file-readable-p element))
          (setq list (cons element list))))))

(defun ff/buffer-exists (file)
  "Returns nil if buffer does 
   not exist"
  (if (get-buffer file)
      t))
