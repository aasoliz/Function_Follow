(defun ff/search-open-buffers (extension)
  "Check for open buffers that might have the 
   function, only called if the current file does not"
  (let (list bf)
    (dolist (element (buffer-list) list)
      (if (string= (file-name-extension (buffer-name element))
                   extension)
          (setq list (cons element list))))
    (dolist (element list bf)
      (set-buffer element)

(ff/search-open-buffers "java")
