(defun ff/search-open-buffers (extension)
  "Check for open buffers that might have the 
   function, only called if the current file does not"
  (let (list bf)
    (dolist (element (buffer-list) list)
      (if (string= (file-name-extension (buffer-name element))
                   extension)
          (setq list (cons element list))))))

(defun ff/displaying-buffer (buffer position)
  "Checks if the buffer is already
   displayed. If so, switch to that
   buffer"
  (let ((bf-window (get-buffer-window buffer)))
    (if (not (eq bf-window nil))
        (progn
          (select-window bf-window)
          (set-window-point bf-window position))
      (switch-to-buffer buffer)))
  (beginning-of-line))

(defun ff/displaying-buffer-with-window (position window)
  "Given the window the buffer is in switch to it
   Similar to 'ff/displaying-buffer' but no need to
   recalculate window"
  (select-window window)
  (set-window-point window position)
  (beginning-of-line))
