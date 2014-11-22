;;; package -- Summary

;;; Commentary:

;;; Code:
(setq imenu-max-item-length 2000)
(setq imenu-space-replacement nil)
(setq scala-imenu:build-imenu-candidate 'ensime-imenu)
(setq scala-imenu:should-flatten-index t)
(defvar ensime-imenu-cache nil)
(add-to-list 'scala-imenu:cleanup-hooks (lambda () (setq ensime-imenu-cache nil)))


(defun ensime-imenu (member-name definition-type marker parents)
  `(,(ensime-imenu-string member-name definition-type marker parents) .
    ,marker))

(defun ensime-imenu-string (member-name definition-type marker parents)
  (let ((scala-imenu-string (car (scala-imenu:default-build-imenu-candidate 
			     member-name definition-type marker parents))))
    (if (equal definition-type "def")
	(let ((ensime-type-string 
	       (get-ensime-type member-name definition-type marker parents)))
	  (message ensime-type-string)
	  (format "%s%s" scala-imenu-string ensime-type-string))
      scala-imenu-string)))

(defun get-ensime-type (member-name definition-type marker parents)
  (let ((parent-name (caar parents))
	(parent-marker (caddar parents)))
    (build-method-type
     (plist-get (type-info member-name parent-name parent-marker) :type))))

(defun build-method-type (type-info)
  (format "%s: %s"
	  (build-argument-lists (plist-get type-info :param-sections))
	  (build-type-string (plist-get type-info :result-type))))

(defun build-argument-lists (param-list-infos)
  (mapconcat 'build-argument-list param-list-infos ""))

(defun build-argument-list (param-list-info)
  (format "(%s)" (mapconcat 'build-argument-string 
			    (plist-get param-list-info :params) ", ")))

(defun build-argument-string (param-info)
  (format "%s: %s" (car param-info)
	  (build-type-string (cadr param-info))))

(defun build-type-string (type-info)
  (let ((name (plist-get type-info :name))
	(type-args (plist-get type-info :type-args)))
    (format "%s%s" name 
	    (if type-args 
		(format "[%s]" (mapconcat 
				'build-type-string type-args ", ")) ""))))

(defun type-info (member-name type-name marker)
  (get-plist-by-keys '(:name) member-name
		     (get-members type-name
				  (get-ensime-type-info-from-mark marker))))

(defun get-members (type-name type-info-plist)
  (plist-get-deep (get-plist-by-keys
		   '(:type :name) type-name
		   (plist-get type-info-plist :interfaces)) '(:type :members)))

(defun plist-get-deep (plist keys)
  (cl-reduce 'plist-get keys :initial-value plist))

(defun get-plist-by-keys (keys value plist)
  (cl-find value plist :test 'equal
	   :key (lambda (member)
		  (plist-get-deep member keys))))

(defun get-ensime-type-info-from-mark (marker)
  (let ((cached-value (assoc marker ensime-imenu-cache)))
    (when (not cached-value)
      (progn (setq cached-value 
		   `(,marker . ,(ensime-rpc-inspect-type-at-range (get-eol-range marker))))
	     (setq ensime-imenu-cache (cons cached-value ensime-imenu-cache))))
    (cdr cached-value)))

(defun get-eol-range (marker)
  (interactive)
  `(,(marker-position marker) 
     ,(save-excursion (goto-char marker) (end-of-line) (point))))

(defun msg-type-info ()
  (interactive)
  (message "%s" (build-method-type 
		 (plist-get (type-info "testFunction" "DFA" (point-marker)) :type))))
