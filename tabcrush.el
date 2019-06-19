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

;;;; Libraries

(require 'cl-lib)
(require 'subr-x)
(require 'thingatpt)

;;;; User options

(defgroup tabcrush nil
  "Major mode for editing very large tables efficiently."
  :group 'applications
  :prefix "tabcrush-")

(defcustom tabcrush-delimiter "|"
  "String used as the column delimiter for Tabcrush tables."
  :type 'string)

;;;; Errors

(defun tabcrush--no-cell ()
  "Signal a user error that point is not inside a table cell."
  (user-error "Not inside a table cell"))

(defun tabcrush--no-table ()
  "Signal a user error that there is no table in the buffer."
  (user-error "No table in the buffer"))

;;;; Reading table data

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
    (unless (search-forward tabcrush-delimiter nil 'noerror)
      (tabcrush--no-table))
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

;;;; Aligning tables

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

;;;; Navigating tables

(defun tabcrush--cell-bounds ()
  "Return the bounds of the current cell contents, as a cons cell.
The car is the beginning of the text in the current cell and the
cdr is the end, not including any whitespace. If no cell can be
identified, raise an error.

A cell starts at its left-hand delimiter, and goes to just before
its right-hand delimiter, except for the last cell in a
row (which includes its right-hand delimiter)."
  (cl-block nil
    (save-excursion
      (unless (eolp)
        (condition-case _
            (forward-char)
          (error (tabcrush--no-cell))))
      (when (save-excursion
              (and (search-backward
                    tabcrush-delimiter
                    (save-excursion
                      (beginning-of-line)
                      (point))
                    'noerror)
                   (looking-at
                    (concat
                     (regexp-quote tabcrush-delimiter) "[[:space:]]*$"))))
        (search-backward tabcrush-delimiter nil 'noerror))
      (unless (search-forward tabcrush-delimiter nil 'noerror)
        (tabcrush--no-cell))
      (goto-char (match-beginning 0))
      (unless (re-search-backward "[^[:space:]]" nil 'noerror)
        (tabcrush--no-cell))
      (goto-char (match-end 0))
      (let ((end (point)))
        (unless (search-backward tabcrush-delimiter nil 'noerror)
          (tabcrush--no-cell))
        (goto-char (match-end 0))
        (when (looking-at
               (concat "[[:space:]]*" (regexp-quote tabcrush-delimiter)))
          (let ((beginning (point)))
            ;; If cell is not totally empty, give a space between the
            ;; left-hand delimiter and the field.
            (when (looking-at "[[:space:]]")
              (cl-incf beginning))
            (cl-return (cons beginning beginning))))
        (unless (re-search-forward "[^[:space:]]" nil 'noerror)
          (tabcrush--no-cell))
        (goto-char (match-beginning 0))
        (let ((beginning (point)))
          (cons beginning end))))))

(defun tabcrush--cell-column-index ()
  "Return the index of the column in which the current cell resides."
  (save-excursion
    (let ((eot (save-excursion
                 (end-of-line)
                 (search-backward tabcrush-delimiter nil 'noerror)
                 (point))))
      (while (and (>= (point) eot) (not (bolp)))
        (backward-char))
      (let ((orig (point))
            (index 0))
        (beginning-of-line)
        (dotimes (_ 2)
          (search-forward tabcrush-delimiter nil 'noerror))
        (while (< (- (point) 2) orig)
          (search-forward tabcrush-delimiter nil 'noerror)
          (cl-incf index))
        index))))

(defun tabcrush--goto-column (index)
  "Go to the start of the cell at the given column INDEX in the current row."
  (beginning-of-line)
  (dotimes (_ (1+ index))
    (unless (search-forward tabcrush-delimiter nil 'noerror)
      (tabcrush--no-cell)))
  (tabcrush-beginning-of-cell))

(defun tabcrush-beginning-of-cell ()
  "Go to the left-hand side of the current cell."
  (interactive)
  (goto-char (car (tabcrush--cell-bounds))))

(defun tabcrush-end-of-cell ()
  "Go to the right-hand side of the current cell."
  (interactive)
  (goto-char (cdr (tabcrush--cell-bounds))))

(defun tabcrush-kill-cell ()
  "Copy the contents of the current cell to the kill ring, and delete them."
  (interactive)
  (let ((bounds (tabcrush--cell-bounds)))
    (kill-region (car bounds) (cdr bounds))))

(defun tabcrush-right-cell ()
  "Go to the left-hand side of the cell to the right."
  (interactive)
  (unless (search-forward tabcrush-delimiter nil 'noerror)
    (tabcrush--no-cell))
  (goto-char (match-end 0))
  (goto-char (car (tabcrush--cell-bounds))))

(defun tabcrush-down-cell ()
  "Go to the left-hand side of the cell below."
  (interactive)
  (cl-block nil
    (save-excursion
      (forward-line)
      (unless (search-forward
               tabcrush-delimiter (save-excursion (end-of-line)) 'noerror)
        (cl-return)))
    (let ((index (tabcrush--cell-column-index)))
      (forward-line)
      (tabcrush--goto-column index))))

;;;; Major mode

(defvar tabcrush-mode-map
  (let ((map (make-sparse-keymap "Tabcrush")))
    (define-key map (kbd "M-a") #'tabcrush-beginning-of-cell)
    (define-key map (kbd "M-e") #'tabcrush-end-of-cell)
    (define-key map (kbd "M-k") #'tabcrush-kill-cell)
    (dolist (tab '("TAB" "<tab>"))
      (define-key map (kbd tab) #'tabcrush-right-cell))
    (dolist (ret '("RET" "<return>"))
      (define-key map (kbd ret) #'tabcrush-down-cell))
    map))

(define-derived-mode tabcrush-mode nil "Tabcrush"
  "Major mode for editing very large tables efficiently.

\\{tabcrush-mode-map}"
  (setq-local truncate-lines t)
  (setq-local auto-fill-function nil)
  (face-remap-add-relative
   'header-line `(:height ,(face-attribute 'default :height))))

;;;; Closing remarks

(provide 'tabcrush)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; tabcrush.el ends here
