;;; multi-line.el --- multi-line statements -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: multi line length whitespace programming
;; URL: https://github.com/IvanMalison/multi-line
;; Version: 0.0.0
;; Package-Requires:

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

;; multi-line aims to provide a flexible framework for automatically
;; multi-lining and single-lining function invocations, array and map
;; literals and more. It relies on functions that are defined on a per
;; major mode basis wherever it can so that function correctly across
;; many different programming languages.

;;; Code:

(defvar multi-line-find-strategy)
(defvar multi-line-enter-strategy)
(defvar multi-line-multi-line-strategy)
(defvar multi-line-single-line-strategy)

(defun multi-line-lparenthesis-advance ()
  (re-search-forward "[[{(]"))

(defun multi-line-up-list-back ()
  (up-list) (backward-sexp))

(defclass multi-line-forward-sexp-enter-strategy ()
  ((done-regex :initarg :done-regex :initform "[[:space:]]*\[[({]")
   (advance-fn :initarg :advance-fn :initform 'multi-line-lparenthesis-advance)
   (inside-fn :initarg :inside-fn :initform 'multi-line-up-list-back)))

(defmethod multi-line-enter ((enter multi-line-forward-sexp-enter-strategy))
  (condition-case nil
      (while (not (looking-at (oref enter :done-regex)))
        (forward-sexp))
    ('error
     (funcall (oref enter :inside-fn))))
  (funcall (oref enter :advance-fn)))

(defun multi-line-comma-advance ()
  (re-search-forward ","))

(defun multi-line-done-advance ()
  (re-search-forward "[)}\]")
  (backward-char))

(defclass multi-line-forward-sexp-find-strategy ()
  ((split-regex :initarg :split-regex :initform "[[:space:]]*,")
   (done-regex :initarg :done-regex :initform "[[:space:]]*[})]")
   (split-advance-fn :initarg :split-advance-fn :initform
                     'multi-line-comma-advance)))

(defmethod multi-line-should-stop ((strategy multi-line-forward-sexp-find-strategy))
  (cond
   ((looking-at (oref strategy :done-regex)) :done)
   ((looking-at (oref strategy :split-regex)) :candidate)
   (t nil)))

(defmethod multi-line-find-next ((strategy multi-line-forward-sexp-find-strategy))
  (let (last last-point this-point)
    (setq this-point (point))
    (condition-case nil
        (while (and (not (equal this-point last-point))
                           (not (setq last (multi-line-should-stop strategy))))
                 (forward-sexp)
                 (setq last-point this-point)
                 (setq this-point (point)))
      ('error (setq last :done))
      nil)
    (when (equal last :candidate) (funcall (oref strategy :split-advance-fn)))
    last))

(defclass multi-line-never-newline ()
  ((spacer :initarg :spacer :initform " ")))

(defmethod multi-line-respace ((respacer multi-line-never-newline) index markers)
  (when (equal index (- (length markers) 1))
    (multi-line-clear-whitespace-at-point))
  (when (not (or (equal 0 index)
                 (equal index (- (length markers) 1))))
        (insert (oref respacer :spacer))))

(defclass multi-line-always-newline ()
  ((always-first :initarg :skip-first :initform nil)
   (always-last :initarg :skip-last :initform nil)))

(defmethod multi-line-should-newline ((respacer multi-line-always-newline)
                                      index markers)
  (let ((marker-length (length markers)))
    (not (or (and (equal 0 index) (oref respacer :skip-first))
             (and (equal index (- marker-length 1)) (oref respacer :skip-last))))))

(defmethod multi-line-respace ((respacer multi-line-always-newline) index markers)
  (when (multi-line-should-newline respacer index markers)
    (newline-and-indent)))

(defclass multi-line-column-number ()
  ((newline-at :initarg :newline-at :initform 80)
   (newline-respacer :initarg :newline-respacer :initform
                       (make-instance multi-line-always-newline))
   (default-respacer :initarg :default-respacer :initform
     (make-instance multi-line-never-newline))))

(defmethod multi-line-should-newline ((respacer multi-line-column-number)
                                      index markers)
  (let ((marker-length (length markers)))
    (or (and (equal 0 index))
        (and (equal index (- marker-length 1)))
        (and (< (+ index 1) marker-length)
             (save-excursion
               (goto-char (marker-position (nth (+ index 1) markers )))
               (> (current-column) (oref respacer :newline-at)))))))

(defmethod multi-line-respace ((respacer multi-line-column-number) index markers)
  (multi-line-respace
   (if (multi-line-should-newline respacer index markers)
       (oref respacer :newline-respacer)
     (oref respacer :default-respacer)) index markers))

(defun multi-line-get-markers (enter-strategy find-strategy)
  (multi-line-enter enter-strategy)
  (let ((markers (list (point-marker))))
    (nconc markers
          (cl-loop until (equal (multi-line-find-next find-strategy) :done)
                   collect (point-marker)))
    (nconc markers (list (point-marker)))))

(defun multi-line-clear-whitespace-at-point ()
  (interactive)
  (let ((start (point)))
    (re-search-forward "[^[:space:]\n]")
    (backward-char)
    (kill-region start (point))))

(defun multi-line-adjust-whitespace (respacer)
  (let ((markers (multi-line-get-markers multi-line-enter-strategy
                                          multi-line-find-strategy)))
    (cl-loop for marker being the elements of markers using (index i) do
             (goto-char (marker-position marker))
             (multi-line-clear-whitespace-at-point)
             (multi-line-respace respacer i markers))))

;;;###autoload
(defun multi-line-set-default-strategies ()
  (interactive)
  (setq multi-line-find-strategy
        (make-instance multi-line-forward-sexp-find-strategy)
        multi-line-enter-strategy
        (make-instance multi-line-forward-sexp-enter-strategy)
        multi-line-multi-line-strategy
        (make-instance multi-line-column-number)
        multi-line-single-line-strategy
        (make-instance multi-line-never-newline)))

;;;###autoload
(defun multi-line-multiline ()
  (interactive)
  (multi-line-adjust-whitespace multi-line-multi-line-strategy))

;;;###autoload
(defun multi-line-singleline ()
  (interactive)
  (multi-line-adjust-whitespace multi-line-single-line-strategy))

(provide 'multi-line)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;;; multi-line.el ends here
