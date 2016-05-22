(defun ff/search-open-buffers (extension)
  "Check for open buffers that might have the 
   function, only called if the current file does not"
  (let (list bf)
    (dolist (element (buffer-list) list)
      (if (string= (file-name-extension (buffer-name element))
                   extension)
          (setq list (cons element list))))))

(defun ff/display-buffer (buffer position)
  "Checks if the buffer is already
   displayed. If so, switch to that
   buffer"
  (let ((bf-window (get-buffer-window buffer)))
    (if (not (eq bf-window nil))
        (progn
          (select-window bf-window)
          (set-window-point bf-window position))
      (select-window (get-lru-window))
      (switch-to-buffer buffer)))
  (beginning-of-line))
