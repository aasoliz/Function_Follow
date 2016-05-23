;;; find-files.el --- Get a list of files in the current directory

;; Copyright (C) 2016  Andrea A Soliz

;; Author: Andrea A Soliz <and.sol17@gmail.com>

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

;;  Makes sure the file is not already linked to a buffer.

;;; Code:

(defun ff/find-files (extension)
"Use EXTENSION to find other files that could have a function definition."
  (let (list index)
    (dolist (element (directory-files "." nil nil t) list)
      (if (and (not (eq (setq index (search "." element)) nil))
               (string= (substring element (1+ index)) extension)
               (not (ff/buffer-exists element))
               (file-readable-p element))
          (setq list (cons element list))))))

(defun ff/buffer-exists (file)
"Return nil if buffer for FILE does not exist."
  (if (get-buffer file)
      t))

(provide 'find-files)

;;; find-files.el ends here
