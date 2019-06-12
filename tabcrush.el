;;; tabcrush.el --- Crushes table-editing problems. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 6 June 2019
;; Homepage: https://github.com/raxod502/tabcrush
;; Keywords: applications
;; Package-Requires: ((emacs "26"))
;; Version: 0

;;; Commentary:

;; TODO

;; Please see <https://github.com/raxod502/tabcrush> for more
;; information.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

(require 'cl-lib)
(require 'subr-x)
(require 'thingatpt)

(defgroup tabcrush nil
  "Major mode for editing very large tables efficiently."
  :group 'applications
  :prefix "tabcrush-")

(defcustom tabcrush-delimiter "|"
  "String used as the column delimiter for Tabcrush tables."
  :type 'string)

(define-derived-mode tabcrush-mode nil "Tabcrush"
  "Major mode for editing very large tables efficiently."
  (setq-local truncate-lines t)
  (setq-local auto-fill-function nil)
  (face-remap-add-relative
   'header-line `(:height ,(face-attribute 'default :height))))

(defun tabcrush--row-to-list ()
  "Return the contents of the current row as a list of strings."
  (let ((row nil))
    (save-excursion
      (save-restriction
        (widen)
        (narrow-to-region
         (save-excursion
           (beginning-of-line)
           (point))
         (save-excursion
           (end-of-line)
           (point)))
        (goto-char (point-min))
        (save-match-data
          (while (re-search-forward
                  (format "%1$s[[:space:]]*\\(.*?\\)[[:space:]]*%1$s"
                          (regexp-quote tabcrush-delimiter))
                  nil 'noerror)
            (push (match-string 1) row)
            (goto-char (match-end 1))))
        (nreverse row)))))

(defun tabcrush--column-headers ()
  "Return the column headers as a list of strings."
  (save-excursion
    (goto-char (point-min))
    (tabcrush--row-to-list)))

(defun tabcrush--row-to-string (row column-widths)
  "Turn ROW, a list of cell strings, into a string.
The cells are padded to at least COLUMN-WIDTHS, a list of
integers of the same length as ROW."
  (concat
   tabcrush-delimiter
   (string-join
    (cl-mapcar
     (lambda (cell column-width)
       (format
        " %s%s "
        cell
        (make-string (- column-width (length cell)) ? )))
     row column-widths)
    tabcrush-delimiter)
   tabcrush-delimiter))

(defun tabcrush--realign-visible ()
  "Realign currently visible table rows to the same widths.
Modify buffer text and update header line."
  (save-excursion
    (let* ((headers (tabcrush--column-headers))
           (column-widths (mapcar #'length headers))
           (last-line-number
            (progn
              (move-to-window-line (window-body-height))
              (line-number-at-pos)))
           (rows nil))
      ;; Set the header to something that takes one line, before we do
      ;; any window line calculations.
      (setq-local header-line-format "")
      (move-to-window-line 0)
      (while (< (line-number-at-pos) last-line-number)
        (let ((row (tabcrush--row-to-list)))
          (setq column-widths
                (cl-mapcar
                 (lambda (column-size cell)
                   (max column-size (length cell)))
                 column-widths
                 row))
          (push row rows))
        (forward-line))
      (setq rows (nreverse rows))
      (setq-local header-line-format
                  (tabcrush--row-to-string headers column-widths))
      (move-to-window-line 0)
      (while (< (line-number-at-pos) last-line-number)
        (let ((row (pop rows)))
          (delete-region
           (point)
           (save-excursion
             (end-of-line)
             (point)))
          (insert (tabcrush--row-to-string row column-widths)))
        (forward-line)))))

;;;; Closing remarks

(provide 'tabcrush)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; tabcrush.el ends here
