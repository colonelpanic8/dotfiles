;;; multi-line.el --- multi-line statements -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: multi line length whitespace programming
;; URL: https://github.com/IvanMalison/multi-line
;; Version: 0.0.0
;; Package-Requires: ((emacs "24"))

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
;; multi-lining and single-lining function invocations and
;; definitions, array and map literals and more. It relies on
;; functions that are defined on a per major mode basis wherever it
;; can so that it functions correctly across many different
;; programming languages.

;;; Code:

(defvar multi-line-config)

(defun multi-line-lparenthesis-advance ()
  "Advance to the beginning of a statement that can be multi-lined."
  (re-search-forward "[[{(]"))

(defun multi-line-up-list-back ()
  "Go to the beginning of a statement from inside the statement."
  (up-list) (backward-sexp))

(defclass multi-line-forward-sexp-enter-strategy ()
  ((done-regex :initarg :done-regex :initform "[[:space:]]*[[({]")
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
  "Advance to the next comma."
  (re-search-forward ","))

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
  "Get the markers for multi-line candidates for the statement at point.

ENTER-STRATEGY is a class with the method multi-line-enter, and
FIND-STRATEGY is a class with the method multi-line-find-next."
  (multi-line-enter enter-strategy)
  (let ((markers (list (point-marker))))
    (nconc markers
          (cl-loop until (equal (multi-line-find-next find-strategy) :done)
                   collect (point-marker)))
    (nconc markers (list (point-marker)))))

(defun multi-line-clear-whitespace-at-point ()
  "Erase any surrounding whitespace."
  (interactive)
  (re-search-backward "[^[:space:]\n]")
  (forward-char)
  (let ((start (point)))
    (re-search-forward "[^[:space:]\n]")
    (backward-char)
    (kill-region start (point))))

(defun multi-line-adjust-whitespace (respacer)
  "Adjust whitespace using the provided RESPACER."
  (let ((markers (multi-line-get-markers
                  (multi-line-get-enter-strategy multi-line-config)
                  (multi-line-get-find-strategy multi-line-config))))
    (cl-loop for marker being the elements of markers using (index i) do
             (goto-char (marker-position marker))
             ;; (multi-line-clear-whitespace-at-point)
             (multi-line-respace respacer i markers))))

(defclass multi-line-config-manager ()
  ((default-find :initarg :default-find :initform
     (make-instance multi-line-forward-sexp-find-strategy))
   (default-enter :initarg :default-enter :initform
     (make-instance multi-line-forward-sexp-enter-strategy))
   (default-respacer :initarg :default-respacer :initform
     (make-instance multi-line-always-newline))
   (major-mode-to-enter :initarg :major-mode-to-enter :initform (make-hash-table))
   (major-mode-to-find :initarg :major-mode-to-find :initform (make-hash-table))
   (major-mode-to-respacer :initarg :major-mode-to-respacer :initform (make-hash-table))))

(defmethod multi-line-get-find-strategy ((config multi-line-config-manager))
  (or (gethash major-mode (oref config :major-mode-to-find))
      (oref config :default-find)))

(defmethod multi-line-get-enter-strategy ((config multi-line-config-manager))
  (or (gethash major-mode (oref config :major-mode-to-enter))
      (oref config :default-enter)))

(defmethod multi-line-get-respacer-strategy ((config multi-line-config-manager))
  (or (gethash major-mode (oref config :major-mode-to-respacer))
      (oref config :default-respacer)))

(defmethod multi-line-set-find-strategy ((config multi-line-config-manager)
                                         for-mode find-strategy)
  (puthash for-mode find-strategy (oref config :major-mode-to-find)))

(defmethod multi-line-set-enter-strategy ((config multi-line-config-manager)
                                          for-mode enter-strategy)
  (puthash for-mode enter-strategy (oref config :major-mode-to-enter)))

(defmethod multi-line-set-respacer-strategy ((config multi-line-config-manager)
                                          for-mode respacer-strategy)
  (puthash for-mode respacer-strategy (oref config :major-mode-to-respacer)))

(defmethod multi-line-set-default-find ((config multi-line-config-manager) find-strategy)
  (oset config :default-find find-strategy))

(defmethod multi-line-set-default-enter ((config multi-line-config-manager)
                                         enter-strategy)
  (oset config :default-enter enter-strategy))

(defmethod multi-line-set-default-respacer ((config multi-line-config-manager)
                                            respacer-strategy)
  (oset config :default-respacer respacer-strategy))

(setq multi-line-config (make-instance multi-line-config-manager))

(defun multi-line-lisp-advance-fn ()
  "Advance to the start of the next multi-line split for Lisp."
  (re-search-forward "[^[:space:]\n]")
  (backward-char))

(defun multi-line-set-per-major-mode-strategies ()
  "Set language specific strategies."
  (interactive)
  (multi-line-set-find-strategy  multi-line-config  'emacs-lisp-mode
                                 (make-instance
                                  multi-line-forward-sexp-find-strategy
                                  :split-regex   "[[:space:]]+"
                                  :done-regex  "[[:space:]]*)"
                                  :split-advance-fn  'multi-line-lisp-advance-fn))

  (let ((newline-respacer
         (make-instance multi-line-always-newline
                        :skip-first t :skip-last t)))
    (multi-line-set-respacer-strategy
     multi-line-config 'emacs-lisp-mode (make-instance  multi-line-column-number
                                                        :newline-respacer
                                                        newline-respacer)))

  ;; No match for done regex
  (multi-line-set-enter-strategy  multi-line-config  'emacs-lisp-mode
                                  (make-instance
                                   multi-line-forward-sexp-enter-strategy
                                   :done-regex "``````")))

(multi-line-set-per-major-mode-strategies)

;;;###autoload
(defun multi-line ()
  "Multi-line the statement at point."
  (interactive)
  (multi-line-adjust-whitespace (multi-line-get-respacer-strategy
                                 multi-line-config)))

;;;###autoload
(defun multi-line-singleline ()
  "Single-line the statement at point."
  (interactive)
  (multi-line-adjust-whitespace (make-instance multi-line-never-newline)))

(provide 'multi-line)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;;; multi-line.el ends here
