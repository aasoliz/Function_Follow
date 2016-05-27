;;; function-follow.el --- Find a function's definition

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

;; Find where a function is defined in source code.

;; To enable:

;;    (require 'function-follow)

;; Setup keybindings or call function:

;;    (global-set-key (kbd "C-c C-j") 'ff/function-follow)
;;    M-x ff/function-follow

;; Options:

;; To control the places  that are searched in order to
;; find the function definition.  Set the variable
;; ff/depth.

;; Default is nil, which only checks the current file

;; t searches through any open buffers

;; files searches through files in the current directory.
;;   This option can be slow when there are a lot of files to search through.

;; Supported Languages:

;; Java, ELisp, Python, Perl, Ruby

;;; Code:

(require 'find-buffers)
(require 'find-files)

;; Keywords specific to functions in each supported language
(defconst ff/definition-keywords '((java-mode ("public" "private"))
                                   (lisp-mode ("defun"))
                                   (emacs-lisp-mode ("defun"))
                                   (python-mode ("def"))
                                   (ruby-mode ("def"))
                                   (perl-mode ("sub"))))

(defcustom ff/depth nil
  "Specifies where to search."
  :group 'Files)

(defun ff/function-follow (point-mark mark-region)
  "Use POINT-MARK and MARK-REGION to find the highlighted function's definition.
First check the current file, then any open buffers with the same file
extension, and finally any files in the current directory with the
same file extension."
  (interactive "r")
  (deactivate-mark)
  (cl-block follow
    (if (or (char-equal ?\( (char-after mark-region))
            (char-equal ?\( (char-after (1- mark-region)))
            (char-equal ?\( (char-before point-mark)))
        (let (position
              window
              (extension (file-name-extension (buffer-name)))
              (regex
               (ff/get-major-mode-keywords
                (buffer-substring-no-properties point-mark mark-region))))
          ;; Search current file
          (if (or (re-search-backward regex nil t) (re-search-forward regex nil t))
              (progn
                (beginning-of-line)
                (cl-return-from follow)))
          ;; Search open buffers
          (if ff/depth
              (dolist (element (ff/find-open-buffers extension) nil)
                (set-buffer element)
                ;; Check backward in case mark is not placed at the top.
                (if (or (setq position (re-search-forward regex nil t))
                        (setq position (re-search-backward regex nil t)))
                    (progn
                      (ff/display-buffer element position)
                      (cl-return-from follow)))))
          ;; Start searching files
          (if (string= ff/depth "files")
              (dolist (element (ff/find-files extension) nil)
                (set-buffer (find-file element))
                ;; Only need to search forward because mark will always start at the top.
                (if (setq position (re-search-forward regex nil t))
                    (progn
                      (ff/display-buffer element position)
                      (cl-return-from follow))
                  (kill-buffer element))))
          (message "Could not find the function"))
      (message "Did not detect method call"))))

(defun ff/assemble-regex (function mode &optional stop)
  "Assemble the regex to find the FUNCTION using the keywords from MODE.
STOP is used to identify which languages have different
conventions in function definitions."
  (let ((rx "\\(") (list))
    (dolist (element mode list)
      (setq rx
            (concat rx (mapconcat 'identity (cons element list) " ") "\\|")))
    (if (string= stop "perl")
        (concat (substring rx 0 -2) "\\).* " (replace-regexp-in-string " " "" function) ".*{")
      (concat (substring rx 0 -2) "\\).* " (replace-regexp-in-string " " "" function) " ?("))))

(defun ff/get-major-mode-keywords (function)
  "Pass along FUNCTION and the major mode keywords.
Assemble the regex inorder to find the function defintion."
  (let ((keywords (assoc major-mode ff/definition-keywords)))
    (unless keywords
      (error "Not a supported major mode"))
    (pcase major-mode
      (`perl-mode        (ff/assemble-regex function keywords "perl"))
      (_                 (ff/assemble-regex function keywords)))))

(provide 'function-follow)

;;; function-follow.el ends here
