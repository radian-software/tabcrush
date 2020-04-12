;;; tabcrush.el --- Crushes table-editing problems. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 6 June 2019
;; Homepage: https://github.com/raxod502/tabcrush
;; Keywords: applications
;; Package-Requires: ((emacs "26"))
;; Version: 0

;;; Commentary:

;; Tabcrush is a package for very-large-scale table editing. It can
;; handle tables of many thousands of lines without performance
;; problems, by performing realignment only as necessary and only on
;; currently visible lines.
;;
;; Tabcrush provides a major mode for table editing. In this mode,
;; literally everything is interpreted as a table. Depending on the
;; appearances of the `tabcrush-delimiter' string, the buffer is
;; partitioned into empty or non-empty cells, which contain text
;; content. Also, a Tabcrush buffer may have associated with it a list
;; of column names. These names appear in the window's header line.
;;
;; The rules for determining how a buffer is partitioned into cells
;; are as follows. Firstly,

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
  "String used as the column delimiter for Tabcrush tables.
May not contain whitespace, but any other characters are allowed."
  :type 'string)

;;;; Errors

(define-error
  'tabcrush-error "Tabcrush: user error" 'user-error)
(define-error
  'tabcrush-search-failed "Tabcrush: search failed" 'tabcrush-error)
(define-error
  'tabcrush-no-cell "Tabcrush: not inside cell" 'tabcrush-error)
(define-error
  'tabcrush-no-table "Tabcrush: not inside table" 'tabcrush-error)

(defmacro tabcrush--signal (symbol &optional data)
  "Signal a Tabcrush user error.
SYMBOL (unquoted) is prepended with `tabcrush-' to make the error
symbol for `signal'. DATA is evaluated and passed to `signal'."
  (let ((error-symbol (intern (format "tabcrush-%S" symbol))))
    (unless (get error-symbol 'error-message)
      (error "No such error: %S" error-symbol))
    `(signal ',error-symbol ,data)))

;;;; Text navigation

(defun tabcrush--bol ()
  "Return the position of the beginning of the current line, as an integer."
  (save-excursion
    (beginning-of-line)
    (point)))

(defun tabcrush--eol ()
  "Return the position of the end of the current line, as an integer."
  (save-excursion
    (end-of-line)
    (point)))

;;;; Delimiter handling

(defun tabcrush--before-delimiter-p ()
  "Return non-nil if `tabcrush-delimiter' is immediately after point.
This function modifies the match data."
  (looking-at (regexp-quote tabcrush-delimiter)))

(defun tabcrush--after-delimiter-p ()
  "Return non-nil if `tabcrush-delimiter' is immediately before point.
This function modifies the match data."
  (save-excursion
    (backward-char (length tabcrush-delimiter))
    (tabcrush--before-delimiter-p)))

(cl-defun tabcrush--back-to-delimiter (side &key same-line)
  "Move point backwards to the nearest occurrence of `tabcrush-delimiter'.
If SIDE is `beginning', move point to the beginning of the
delimiter. If SIDE is `end', move point to the end of it.

If SAME-LINE is non-nil, only consider delimiters that occur on
the same line as point.

If no delimiter is found, signal `tabcrush-search-failed'.

This function modifies the match data."
  (unless (search-backward
           tabcrush-delimiter (when same-line (tabcrush--bol)) 'noerror)
    (tabcrush--signal search-failed))
  (pcase side
    (`beginning)
    (`end (goto-char (match-end 0)))
    (_ (error "Invalid SIDE: %S" side))))

(cl-defun tabcrush--forward-to-delimiter (side &key same-line)
  "Move point forwards to the nearest occurrence of `tabcrush-delimiter'.
If SIDE is `beginning', move point to the beginning of the
delimiter. If SIDE is `end', move point to the end of it.

If SAME-LINE is non-nil, only consider delimiters that occur on
the same line as point.

If no delimiter is found, signal `tabcrush-search-failed'.

This function modifies the match data."
  (unless (search-forward
           tabcrush-delimiter (when same-line (tabcrush--eol)) 'noerror)
    (tabcrush--signal search-failed))
  (pcase side
    (`beginning (goto-char (match-beginning 0)))
    (`end)
    (_ (error "Invalid SIDE: %S" side))))

;;;; Table geometry

(defun tabcrush--inside-cell-p ()
  "Return non-nil if point is inside a cell.

This function modifies the match data."
  (save-excursion
    (beginning-of-line)
    (condition-case _
        (dotimes (_ 2 t)
          (tabcrush--forward-to-delimiter 'end :same-line t))
      (tabcrush-search-failed))))

(defun tabcrush--borp ()
  "Return non-nil if point is at the beginning of the current row.
This means that point is farther to the left than it would be if
it were `looking-at' the first delimiter in the row.")
;; FIXME: finish this and use it

(defun tabcrush--eorp ()
  "Return non-nil if point is at the end of the current row.
This means that point is `looking-at' the final delimiter in the
row, or is farther to the right.

If point is not inside a cell, raise an error.

This function modifies the match data."
  (unless (tabcrush--inside-cell-p)
    (tabcrush--signal no-cell))
  (save-excursion
    (unless (eolp)
      (forward-char))
    (condition-case _
        (prog1 nil
          (tabcrush--forward-to-delimiter 'end :same-line t))
      (tabcrush-search-failed t))))

(defun tabcrush--cell-bounds ()
  "Return the bounds of the current cell.
Return the bounds as a cons cell, where the car is the beginning
of the region of the buffer which is considered part of the
current cell (inclusive) and the cdr is the end (exclusive).

If point is not inside a table cell, raise an error.

This function modifies the match data."
  (save-excursion
    (condition-case _
        (let ((eorp (tabcrush--eorp)))
          (when eorp
            (if (tabcrush--looking-at-delimiter)
                (condition-case _
                    (if (bolp)
                        (tabcrush--signal no-cell)
                      (backward-char))
                  (beginning-of-buffer (tabcrush--signal no-cell)))
              (tabcrush--back-to-delimiter 'beginning :same-line t)))
          (unless (and (null eorp) (tabcrush--looking-at-delimiter))
            (tabcrush--back-to-delimiter 'beginning :same-line t))
          (let ((beginning (point)))
            (tabcrush--forward-to-delimiter 'end :same-line t)
            (tabcrush--forward-to-delimiter 'beginning :same-line t)
            (when (tabcrush--eorp)
              (end-of-line)
              (condition-case _
                  (forward-char)
                (end-of-buffer)))
            (let ((end (point)))
              (cons beginning end))))
      (tabcrush-search-failed (tabcrush--signal no-cell)))))

(defun tabcrush--cell-text-bounds ()
  "Return the bounds of the text contents of the cell.
This is the same as `tabcrush--cell-bounds', except that the cons
cell bounds the text contents only, rather than the entire
cell (which also includes spacing and one or both delimiters).

This function modifies the match data."
  (cl-block nil
    (save-excursion
      (let ((bounds (tabcrush--cell-bounds)))
        (goto-char (car bounds))
        (tabcrush--forward-to-delimiter 'end :same-line t)
        (let ((orig (point)))
          (if (< (re-search-forward "[^[:space:]]") (cdr bounds))
              (goto-char (match-beginning 0))
            (goto-char orig)
            (when (looking-at "[[:space:]]")
              (forward-char))
            (cl-return (cons (point) (point)))))
        (let ((beginning (point)))
          (tabcrush--forward-to-delimiter 'beginning :same-line t)
          (re-search-backward "[^[:space:]]")
          (goto-char (match-end 0))
          (let ((end (point)))
            (cons beginning end)))))))

;;;; Reading table data

(defun tabcrush--row-to-list ()
  "Return the contents of the current row as a list of strings.

This function modifies the match data."
  (let ((row nil))
    (save-excursion
      (beginning-of-line)
      (while (progn
               (let ((text-bounds (tabcrush--cell-text-bounds))
                     (cell-bounds (tabcrush--cell-bounds)))
                 (push (buffer-substring
                        (car text-bounds)
                        (cdr text-bounds))
                       row)
                 (goto-char (cdr cell-bounds))
                 (not (string-match "\n" (buffer-substring
                                          (car cell-bounds)
                                          (cdr cell-bounds)))))))
      (nreverse row))))

(defun tabcrush--column-headers ()
  "Return the column headers as a list of strings."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (condition-case _
          (progn
            (tabcrush--forward-to-delimiter 'beginning)
            (tabcrush--row-to-list))
        ((tabcrush-search-failed tabcrush-no-cell)
         (tabcrush--signal no-table))))))

;;;; Aligning tables

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

;;;; Interactive functions
;;;;; Table navigation

(defun tabcrush-left-cell ()
  "Go to the left-hand side of the cell to the left.
If at the far left column and prefix argument is given, then wrap
around to the end of the previous row."
  (interactive)
  (goto-char (car (tabcrush--cell-bounds)))
  (backward-char)
  (goto-char (car (tabcrush--cell-text-bounds))))

(defun tabcrush-right-cell ()
  "Go to the left-hand side of the cell to the right."
  (interactive)
  (unless (search-forward tabcrush-delimiter nil 'noerror)
    (tabcrush--no-cell))
  (when (looking-at "[[:space:]]*$")
    (end-of-line)
    (ignore-errors
      (forward-char)))
  (goto-char (car (tabcrush--cell-bounds))))

(defun tabcrush--cell-column-index ()
  "Return the index of the column in which the current cell resides."
  (save-excursion
    (let ((orig (point)))
      (beginning-of-line)
      (while (< (cdr (tabcrush--cell-bounds)) orig)))
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

;; FIXME: make it wrap around?
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
    (define-key map (kbd "<backtab>") #'tabcrush-left-cell)
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
