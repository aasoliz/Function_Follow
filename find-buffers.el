;;; find-buffers.el --- Get a list of open buffers

;; Copyright (C) 2016  Andrea A Soliz

;; Author: Andrea A Soliz <and.sol17@gmail.com>
;; Keywords: function method

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  Only considers buffers that have the same extension as the original buffer.

;;; Code:

(defun ff/find-open-buffers (extension)
"Find open buffers with the same EXTENSION as the original file."
  (let (list bf)
    (dolist (element (buffer-list) list)
      (if (string= (file-name-extension (buffer-name element))
                   extension)
          (setq list (cons element list))))))

(defun ff/display-buffer (buffer position)
"Switch to BUFFER and set the mark to the POSITION of the function definition."
  (let ((bf-window (get-buffer-window buffer)))
    (if (not (eq bf-window nil))
        (progn
          (select-window bf-window)
          (set-window-point bf-window position))
      (select-window (get-lru-window))
      (switch-to-buffer buffer)))
  (beginning-of-line))

(provide 'find-buffers)

;;; find-buffers.el ends here
