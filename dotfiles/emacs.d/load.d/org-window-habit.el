(require 'eieio)
(require 'calendar)
(require 'org)
(require 'org-habit)
(require 'cl-lib)

(defun org-window-habit-time-max (&rest args)
  "Return the maximum time value from ARGS."
  (let ((max-time (car args)))
    (dolist (time (cdr args))
      (when (time-less-p max-time time)
        (setq max-time time)))
    max-time))

(defun org-window-habit-negate-plist (plist)
  (org-window-habit-multiply-plist plist -1))

(defun org-window-habit-multiply-plist (plist factor)
  (let (result)
    (while plist
      (let ((key (pop plist))
            (value (pop plist)))
        (push key result)
        (push (* factor value) result)))
    (nreverse result)))

(defun org-window-habit-duration-proportion (start-time end-time between-time)
  (let* ((full-interval (float-time (time-subtract end-time start-time)))
         (partial-interval (float-time (time-subtract end-time between-time))))
    (/ partial-interval full-interval)))

(cl-defun org-window-habit-keyed-duration-add
    (&key (base-time (current-time))
          (days 0) (months 0) (years 0)
          (hours 0) (minutes 0) (seconds 0))
  (let* ((decoded-base (decode-time base-time))
         (base-year (nth 5 decoded-base))
         (base-month (nth 4 decoded-base))
         (base-day (nth 3 decoded-base))
         (base-absolute (calendar-absolute-from-gregorian (list base-month base-day base-year)))
         (new-absolute (+ base-absolute days))
         (gregorian-result (calendar-gregorian-from-absolute new-absolute))
         (result-year (+ (caddr gregorian-result) years))
         (result-month (+ (car gregorian-result) months)))

    ;; Handle month overflows and underflows
    (while (> result-month 12)
      (setq result-month (- result-month 12)
            result-year (+ result-year 1)))

    (while (< result-month 1)
      (setq result-month (+ result-month 12)
            result-year (- result-year 1)))

    (encode-time (+ seconds (nth 0 decoded-base))
                 (+ minutes (nth 1 decoded-base))
                 (+ hours (nth 2 decoded-base))
                 (cadr gregorian-result)
                 result-month
                 result-year)))

(defun org-window-habit-keyed-duration-add-plist (base-time plist)
  (apply 'org-window-habit-keyed-duration-add :base-time base-time plist))

(cl-defun org-window-habit-string-duration-to-plist
    (string-value &key (default nil))
  (if (null string-value)
      default
    (let ((read-value (read string-value)))
      (cond
       ((plistp read-value) read-value)
       ((string-match "\\([0-9]+\\)[Yy]" string-value)
        (list :years (string-to-number (match-string 1 string-value))))

       ;; Month pattern
       ((string-match "\\([0-9]+\\)[Mm]" string-value)
        (list :months (string-to-number (match-string 1 string-value))))

       ;; Week pattern
       ((string-match "\\([0-9]+\\)[Ww]" string-value)
        (list :days (* 7 (string-to-number (match-string 1 string-value)))))

       ;; Day pattern
       ((string-match "\\([0-9]+\\)[Dd]" string-value)
        (list :days (string-to-number (match-string 1 string-value))))

       ;; Hour pattern
       ((string-match "\\([0-9]+\\)[Hh]" string-value)
        (list :hours (string-to-number (match-string 1 string-value))))
       (t (list :days read-value))))))

(defun org-window-habit-normalize-time-to-duration
    (time-value &optional duration-plist alignment-time)
  (let* ((alignment-decoded (decode-time (or alignment-time time-value)))
         (year (nth 5 alignment-decoded))
         (month (nth 4 alignment-decoded))
         (day (nth 3 alignment-decoded))
         (hour (nth 2 alignment-decoded))
         (minute (nth 1 alignment-decoded))
         (second (nth 0 alignment-decoded))
         (smallest-duration-type (car (last duration-plist 2)))
         (smallest-duration-value (cadr (last duration-plist 2))))

    ;; Align time based on the smallest duration type and its value
    (cond
     ((eq smallest-duration-type :seconds)
      (encode-time
       (* smallest-duration-value (floor second smallest-duration-value)) minute
       hour day month year))

     ((eq smallest-duration-type :minutes)
      (encode-time
       0 (* smallest-duration-value
            (floor minute smallest-duration-value)) hour day month year))

     ((eq smallest-duration-type :hours)
      (encode-time
       0 0 (* smallest-duration-value (floor hour smallest-duration-value))
       day month year))

     ((eq smallest-duration-type :days)
      (let* ((aligned-day (- day (1- smallest-duration-value))))
        (encode-time 0 0 0 aligned-day month year)))

     ((eq smallest-duration-type :months)
      (encode-time 0 0 0 1
                   (* smallest-duration-value (floor month smallest-duration-value))
                   year))

     ((eq smallest-duration-type :years)
      (let* ((aligned-year (- year (1- smallest-duration-value))))
        (encode-time 0 0 0 1 1 aligned-year)))

     (t time-value))))

(defun org-window-habit-find-aligned-bounding-time (time-value duration-plist aligned-time)
  (let (new-time)
    (while (time-less-p time-value aligned-time)
      (setq new-time
            (org-window-habit-keyed-duration-add-plist aligned-time duration-plist))
      (when (not (time-less-p new-time aligned-time))
        (error "Time did not decrease in alignment attempt"))
      (setq aligned-time new-time)))
  aligned-time)

(defun org-window-habit-logbook-drawer-bounds ()
  (when (re-search-forward org-logbook-drawer-re nil t)
    (list (match-beginning 0) (match-end 0))))

(defun org-window-habit-parse-logbook ()
  (let ((bounds (org-window-habit-logbook-drawer-bounds)))
    (when bounds
      (cl-destructuring-bind (start end) bounds
        (goto-char start)
        (let ((re (org-window-habit-get-logbook-entry-re)))
          (cl-loop while (re-search-forward re end t)
                   collect (list
                            (match-string-no-properties 1)
                            (match-string-no-properties 2)
                            (org-time-string-to-time (match-string-no-properties 3)))))))))

(defun org-window-habit-get-logbook-entry-re (&optional state-regexp)
  (unless state-regexp
    (setq state-regexp (rx alphanumeric)))
  (rx
   (: line-start (* space) "-" (* space))
   "State" (* space) (? "\"") (group (* alphanumeric)) (? "\"")
   (* space) (? (: "from" (* space) (? "\"") (group (* alphanumeric)) (? "\"")))
   (* space)
   (regexp org-ts-regexp-inactive)))

(defclass org-window-habit ()
  ((duration-plist :initarg :duration-plist :initform '(:days 1))
   (assessment-interval :initarg :assessment-interval :initform '(:days 1))
   (reschedule-interval :initarg :reschedule-interval :initform '(:days 1))
   (repetitions-required :initarg :repetitions-required :initform 1)
   (okay-repetitions-required :initarg :okay-repetitions-required :initform 1)
   (done-times :initarg :done-times :initform nil)
   (window-decrement-plist :initarg :window-decrement-plist :initform nil)
   (max-repetitions-per-interval :initarg :max-repetitions-per-interval :initform 1)))

(defun org-window-habit-create-instance-from-heading-at-point ()
  "Construct an org-window-habit instance from the current org entry."
  (save-excursion
    (let* ((done-times
            (cl-loop for state-change-info in (org-window-habit-parse-logbook)
                     if (member (nth 0 state-change-info) org-done-keywords)
                     collect (nth 2 state-change-info)))
           (done-times-vector (vconcat done-times))
           (window-length
            (org-window-habit-string-duration-to-plist
             (org-entry-get nil "WINDOW_DURATION" "1d") :default '(:days 1)))
           (assessment-interval
            (org-window-habit-string-duration-to-plist
             (org-entry-get nil "ASSESMENT_INTERVAL") :default '(:days 1)))
           (reschedule-interval
            (org-window-habit-string-duration-to-plist
             (org-entry-get nil "RESCHEDULE_INTERVAL")))
           (repetitions-required
            (string-to-number
             (or (org-entry-get nil "REPETITIONS_REQUIRED" t) "1")))
           (okay-repetitions-required
            (string-to-number
             (or (org-entry-get nil "OKAY_REPETITIONS_REQUIRED" t) "1")))
           (max-repetitions-per-interval
            (string-to-number
             (or (org-entry-get nil "MAX_REPETITIONS_PER_INTERVAL" t) "1"))))
      (make-instance 'org-window-habit
                     :duration-plist window-length
                     :assessment-interval assessment-interval
                     :reschedule-interval reschedule-interval
                     :repetitions-required repetitions-required
                     :okay-repetitions-required okay-repetitions-required
                     :done-times done-times-vector
                     :max-repetitions-per-interval max-repetitions-per-interval))))

(cl-defmethod initialize-instance :after ((habit org-window-habit) &rest _args)
  (when (null (oref habit assessment-interval))
    (oset habit assessment-interval (oref habit duration-plist)))
  (when (null (oref habit reschedule-interval))
    (oset habit reschedule-interval (oref habit reschedule-interval)))
  (when (null (oref habit window-decrement-plist))
    (oset habit window-decrement-plist
          (org-window-habit-negate-plist (oref habit assessment-interval)))))

(cl-defun org-window-habit-find-array-forward
    (array time &key (start-index nil) (comparison '<))
  (setq start-index (or start-index 0))
  (cl-loop for index from start-index to (length array)
           while (and (< index (length array))
                      (funcall comparison time (aref array index)))
           finally return index))

(cl-defun org-window-habit-find-array-backward
    (array time &key (start-index nil) (comparison '<))
  (setq start-index (or start-index (length array)))
  (cl-loop for index downfrom start-index to 1
           for testing-value = (aref array (- index 1))
           while (funcall comparison time testing-value)
           finally return index))

(defun time-less-or-equal-p (time1 time2)
  (or (time-less-p time1 time2)
      (time-equal-p time1 time2)))

(defun time-greater-p (time1 time2)
  (time-less-p time2 time1))

(defun time-greater-or-equal-p (time1 time2)
  (time-less-or-equal-p time2 time1))

(cl-defmethod org-window-habit-get-completion-window-indices
  ((habit org-window-habit) start-time end-time
   &key (start-index nil) (end-index nil) (reverse nil))
  (with-slots (done-times) habit
    (if (not reverse)
        (list
         ;; We use end-time and not start time because the array is in descending
         ;; order
         (org-window-habit-find-array-forward
          done-times end-time
          ;; This actually makes this value exclusive because this function
          ;; will now pass over equal values
          :comparison 'time-less-or-equal-p
          :start-index start-index)
         ;; We use start-time to compute the end index because the list is in
         ;; descending order
         (org-window-habit-find-array-forward
          done-times start-time
          ;; Again, this is counter-intuitive but using strict less here
          ;; actually makes this interval inclusive
          :comparison 'time-less-p
          :start-index end-index))
      (list
       ;; We use end-time and not start time because the array is in descending
       ;; order
       (org-window-habit-find-array-backward
        done-times end-time
        ;; Here, because we are searching backwards, this actually does the more
        ;; intuitve thing of giving us an exclusive bound index.
        :comparison 'time-greater-p
        :start-index start-index)
       ;; We use start-time to compute the end index because the list is in
       ;; descending order
       (org-window-habit-find-array-backward
        done-times start-time
        ;; Here, because we are searching backwards, this actually does the more
        ;; intuitve thing of giving us an exclusive bound index.
        :comparison 'time-greater-or-equal-p
        :start-index end-index)))))

;; TODO avoid using current-time
(cl-defmethod org-window-habit-get-windows
  ((window-habit org-window-habit) &key (max-intervals nil))
  (with-slots (duration-plist done-times window-decrement-plist) window-habit
    (let* ((done-times-count (length done-times))
           (earliest-completion (when (> done-times-count 0)
                                  (aref done-times (- done-times-count 1)))))
      (when earliest-completion
        (cl-loop
         with start-index = 0
         with end-index = 0
         with interval-ongoing = t
         with current-window-start =
         (org-window-habit-normalize-time-to-duration (current-time) duration-plist)
         for current-window-end =
         (org-window-habit-keyed-duration-add-plist current-window-start duration-plist)
         for (new-start-index new-end-index) =
         (org-window-habit-get-completion-window-indices
          window-habit current-window-start current-window-end
          :start-index start-index :end-index (or end-index 0))
         for last-start = current-window-start
         do
         (setq start-index new-start-index
               end-index new-end-index)
         when (>= start-index done-times-count)
         return windows
         for effective-start =
         (if (time-less-p earliest-completion current-window-start)
             current-window-start
           (org-window-habit-time-max
            current-window-start
            (org-window-habit-find-aligned-bounding-time
             earliest-completion window-decrement-plist current-window-end)))
         collect
         (list current-window-start effective-start current-window-end
               start-index end-index interval-ongoing)
         into windows
         do (setq interval-ongoing nil)
         when (and max-intervals (>= (length windows) max-intervals))
         return windows
         do
         (setq current-window-start
               (org-window-habit-keyed-duration-add-plist
                current-window-start
                window-decrement-plist))
         when (not (time-less-p current-window-start last-start))
         do (error "The window start did not get smaller"))))))

(cl-defmethod org-window-habit-advance-window
  ((window-habit org-window-habit) start-time end-time &key (direction 1))
  (with-slots (assessment-interval) window-habit
    (let ((interval-movement-plist (org-window-habit-multiply-plist
                                    assessment-interval direction)))
      (list
       (org-window-habit-keyed-duration-add-plist start-time interval-movement-plist)
       (org-window-habit-keyed-duration-add-plist end-time interval-movement-plist)))))

(defvar org-window-habit-face-fn 'org-window-habit-default-face-fn)

(defface org-window-habit-conformed-with-completion-face
  '((((background light)) (:background "#40778f"))
    (((background dark)) (:background "#40778f")))
  "Face for intervals for which the user was conforming only with their completion."
  :group 'org-window-habit
  :group 'org-faces)

(defface org-window-habit-conforming-without-completion-face
  '((((background light)) (:background "#40578f"))
    (((background dark)) (:background "#40578f")))
  "Face for intervals for which the user is conforming without any completions."
  :group 'org-window-habit
  :group 'org-faces)

(defface org-window-habit-conforming-with-completion-face
  '((((background light)) (:background "#f5f946"))
    (((background dark)) (:background "gold")))
  "Face for currently ongoing interval where user is conforming with completion."
  :group 'org-window-habit
  :group 'org-faces)

(defface org-window-habit-okay-conforming-face
  '((((background light)) (:background "#FF00FF"))
    (((background dark)) (:background "#FF00FF")))
  "Face for interval in which the user is only okay conforming ."
  :group 'org-window-habit
  :group 'org-faces)

(defface org-window-habit-extreme-not-conforming-face
  '((((background light)) (:background "#fc9590"))
    (((background dark)) (:background "darkred")))
  "Face for interval in which the user is not conforming by a large ."
  :group 'org-window-habit
  :group 'org-faces)

(defface org-window-habit-not-conforming-face
  '((((background light)) (:background "#f9372d"))
    (((background dark)) (:background "firebrick")))
  "Face for interval in which the user is not conforming."
  :group 'org-window-habit
  :group 'org-faces)

(cl-defun org-window-habit-default-face-fn
    (perfect-repetitions-required
     okay-repetitions-required
     completions-without-interval
     completions-in-interval
     interval-ongoing &key
     (completions-per-interval 1))
    (cond
     ((>= completions-without-interval perfect-repetitions-required)
      'org-window-habit-conforming-without-completion-face)
     ((>= (+ completions-without-interval completions-in-interval) perfect-repetitions-required)
      'org-window-habit-conformed-with-completion-face)
     ((and interval-ongoing
           (>= (+ completions-without-interval completions-per-interval)
               perfect-repetitions-required))
      'org-window-habit-conforming-with-completion-face)
     ((>= (+ completions-without-interval completions-in-interval)
          okay-repetitions-required)
      'org-window-habit-okay-conforming-face)
     (t 'org-window-habit-not-conforming-face)))

(defcustom org-window-habit-preceding-intervals 21
  "Number of days before today to appear in consistency graphs."
  :group 'org-window-habit
  :type 'integer)

(defcustom org-window-habit-following-days 7
  "Number of days after today to appear in consistency graphs."
  :group 'org-window-habit
  :type 'integer)

(cl-defmethod org-window-habit-build-graph ((habit org-window-habit))
  (with-slots
      (duration-plist repetitions-required okay-repetitions-required window-decrement-plist)
      habit
    (let* ((past-and-present-windows
            (nreverse (org-window-habit-get-windows
                       habit :max-intervals org-window-habit-preceding-intervals)))
           (filler-count (- org-window-habit-preceding-intervals
                            (length past-and-present-windows))))
      (nconc
       (cl-loop for i from 0 to filler-count
                collect (list ?\s 'org-window-habit-conforming-without-completion-face))
       (cl-loop
        for (start-time actual-start-time end-time start-index end-index interval-ongoing)
        in past-and-present-windows
        for duration-proportion =
        (org-window-habit-duration-proportion
         start-time end-time actual-start-time)
        for scaled-repetitions-required =
        (* duration-proportion repetitions-required)
        for scaled-okay-repetitions-required =
        (* duration-proportion okay-repetitions-required)
        for interval-start-time =
        (org-window-habit-keyed-duration-add-plist
         end-time window-decrement-plist)
        for (interval-start-index interval-end-index) =
        (org-window-habit-get-completion-window-indices
         habit interval-start-time end-time
         :start-index start-index :end-index start-index)
        for strict-completions =
        (org-window-habit-get-completion-count
         habit start-time end-time :start-index start-index)
        for total-completions = (- end-index start-index)
        for completions-in-interval = (- interval-end-index interval-start-index)
        for completions-outside-interval = (- total-completions completions-in-interval)
        for face =
        (funcall org-window-habit-face-fn
                 scaled-repetitions-required
                 scaled-okay-repetitions-required
                 completions-outside-interval
                 completions-in-interval
                 interval-ongoing)
        for character =
        (cond
         ((>= completions-in-interval 1) org-habit-completed-glyph)
         (interval-ongoing org-habit-today-glyph)
         (t ?\s))
        collect (list character face))))))

(defun org-window-habit-make-graph-string (graph-info)
  (let ((graph (make-string (length graph-info) ?\s)))
    (cl-loop for (character face) in graph-info
             for index from 0
             do
             (progn
               (aset graph index character)
               (put-text-property index (1+ index) 'face face graph)))
    graph))

(define-minor-mode org-window-habit-mode
  "Minor mode that replaces the normal org-habit functionality."
  :lighter nil
  :global t
  :group 'org-window-habit
  :require 'org-window-habit)

(defun org-window-habit-parse-todo ()
  (org-window-habit-create-instance-from-heading-at-point))

(defun org-window-habit-parse-todo-advice (orig &rest args)
  (if org-window-habit-mode
      (org-window-habit-parse-todo)
    (apply orig args)))

(advice-add 'org-habit-parse-todo
            :around 'org-window-habit-parse-todo-advice)

(defun org-window-habit-insert-consistency-graphs-advice (orig &rest args)
  (if org-window-habit-mode
      (org-window-habit-insert-consistency-graphs)
    (apply orig args)))

(advice-add 'org-habit-insert-consistency-graphs
            :around 'org-window-habit-insert-consistency-graphs-advice)

(defun org-window-habit-get-urgency-advice (orig &rest args)
  (if org-window-habit-mode
      org-default-priority              ;TODO fix this
    (apply orig args)))

(advice-add 'org-habit-get-urgency
            :around 'org-window-habit-get-urgency-advice)

(defun org-window-habit-show-time-string (time)
  (format-time-string
   "%Y-%m-%d %H:%M"
   time))

(defun org-window-habit-get-next-required-interval-test ()
  (mapcar
   'org-window-habit-show-time-string
   (org-window-habit-get-next-required-interval
    (org-window-habit-create-instance-from-heading-at-point))))

(cl-defmethod org-window-habit-get-completion-count
  ((habit org-window-habit) start-time end-time &key (start-index 0))
  (cl-loop
   with next-start-index = start-index
   with interval-end-time = end-time
   for interval-start-time =
   ;; This is just a sanity check for the case where the interval does not
   ;; evenly divide the window. But you shouldn't do that anyway.
   (org-window-habit-time-max
    start-time
    (org-window-habit-keyed-duration-add-plist
     interval-end-time (oref habit window-decrement-plist)))
   for (start-index end-index) =
   (org-window-habit-get-completion-window-indices
    habit interval-start-time interval-end-time
    :start-index next-start-index
    :end-index next-start-index)
   for completions-within-interval =
   (min (oref habit max-repetitions-per-interval) (- end-index start-index))
   sum completions-within-interval
   do (setq next-start-index end-index
            interval-end-time interval-start-time)
   while (time-less-p start-time interval-start-time)))

(cl-defmethod org-window-habit-get-next-required-interval ((habit org-window-habit))
  (cl-loop
   with
   (start-time effective-start
               end-time last-start-index last-end-index ongoing) =
               (car (org-window-habit-get-windows habit :max-intervals 1))
   with last-end-time =
   (org-window-habit-keyed-duration-add-plist
    end-time
    (oref habit window-decrement-plist))
   with reschedule-decrement-plist =
   (org-window-habit-negate-plist (oref habit reschedule-interval))
   for (start-index end-index) =
   (org-window-habit-get-completion-window-indices
    habit
    start-time end-time
    :start-index last-start-index
    :end-index last-end-index
    :reverse t)
   for actual-completions =
   (org-window-habit-get-completion-count
    habit start-time end-time :start-index start-index)
   for expected-completions = actual-completions
   for actual-start = (org-window-habit-time-max effective-start start-time)
   for proportion =
   (org-window-habit-duration-proportion start-time end-time actual-start)
   for required = (* proportion (oref habit repetitions-required))
   for reschedule-start-time =
   (org-window-habit-keyed-duration-add-plist
    end-time reschedule-decrement-plist)
   for (reschedule-start-index reschedule-end-index) =
   (org-window-habit-get-completion-window-indices
    habit reschedule-start-time end-time
    :start-index start-index
    :end-index end-index
    :reverse t)
   for interval-has-completion = (not (eq reschedule-start-index reschedule-end-index))
   do
   (message
    "h: %s %s %s %s %s"
    interval-has-completion
    old-completions
    actual-completions
    (org-window-habit-show-time-string start-time)
    (org-window-habit-show-time-string end-time))
   until (and (not interval-has-completion) (< expected-completions required))
   for (new-start-time new-end-time) =
   (org-window-habit-advance-window habit start-time end-time)
   do
   (setq last-end-time end-time
         start-time new-start-time
         end-time new-end-time
         last-start-index start-index
         last-end-index end-index)
   finally return (list last-end-time end-time)))

(defun org-window-habit-auto-repeat (&rest args)
  (interactive)
  (let* ((required-interval-start
          (car (org-window-habit-get-next-required-interval
                (org-window-habit-create-instance-from-heading-at-point))))
         (repeat (org-get-repeat))
         (target-time-string
          (format-time-string (car org-timestamp-formats)
                              required-interval-start)))
    (org-deadline nil target-time-string)
    (org-schedule nil target-time-string)))

(defun org-window-habit-auto-repeat-maybe-advice (orig &rest args)
  (let ((res (apply orig args)))
    (when (and org-window-habit-mode (org-is-habit-p))
      (apply 'org-window-habit-auto-repeat args))
    res))

(advice-add 'org-auto-repeat-maybe
            :around 'org-window-habit-auto-repeat-maybe-advice)

;; This seems to be the actually important annotation
(advice-add 'org-add-log-note
            :around 'org-window-habit-auto-repeat-maybe-advice)

(defun org-window-habit-insert-consistency-graphs (&optional line)
  "Insert consistency graph for any habitual tasks."
  (let ((inhibit-read-only t)
	(buffer-invisibility-spec '(org-link)))
    (save-excursion
      (goto-char (if line (line-beginning-position) (point-min)))
      (while (not (eobp))
	(let ((habit (get-text-property (point) 'org-habit-p))
          (invisible-prop (get-text-property (point) 'invisible)))
	  (when habit
	    (move-to-column org-habit-graph-column t)
	    (delete-char (min (+ 1 org-habit-preceding-days
				 org-habit-following-days)
			              (- (line-end-position) (point))))
	    (insert-before-markers
	     (org-window-habit-make-graph-string
          (org-window-habit-build-graph habit))))
      ;; Inherit invisible state of hidden entries.
      ;; (when invisible-prop
      ;;   (put-text-property
      ;;    (- (point) org-habit-graph-column) (point)
      ;;    'invisible invisible-prop))))
	  (forward-line))))))

(provide 'org-window-habit)
