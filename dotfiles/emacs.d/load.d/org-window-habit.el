(require 'eieio)
(require 'calendar)
(require 'org)
(require 'org-habit)
(require 'cl-lib)

(defcustom org-window-habit-preceding-intervals 30
  "Number of days before today to appear in consistency graphs."
  :group 'org-window-habit
  :type 'integer)

(defcustom org-window-habit-following-days 7
  "Number of days after today to appear in consistency graphs."
  :group 'org-window-habit
  :type 'integer)

(defvar org-window-habit-graph-assessment-fn
  'org-window-habit-default-graph-assessment-fn)

(define-minor-mode org-window-habit-mode
  "Minor mode that replaces the normal org-habit functionality."
  :lighter nil
  :global t
  :group 'org-window-habit
  :require 'org-window-habit)

(defvar org-window-habit-conforming-color "#0000FF")
(defvar org-window-habit-not-conforming-color "#FF0000")
(defvar org-window-habit-required-completion-foreground-color "#000000")
(defvar org-window-habit-non-required-completion-foreground-color "#FFFFFF")
(defvar org-window-habit-required-completion-today-foreground-color "#00FF00")

(defun org-window-habit-create-face (bg-color foreground-color)
  (let* ((bg-name (replace-regexp-in-string "#" "" bg-color))
         (fg-name (replace-regexp-in-string "#" "" foreground-color))
         (face-name (intern (format "org-window-habit-face-bg-%s-fg-%s" bg-name fg-name))))
    (if (facep face-name)
        face-name
      (progn
        (make-face face-name)
        (set-face-attribute face-name nil :background bg-color :foreground foreground-color)
        face-name))))

(defcustom org-window-habit-completion-needed-today-glyph ?▂
  "Glyph character used to show days on which a completion is expected."
  :group 'org-habit
  :version "24.1"
  :type 'character)

(defvar org-window-habit-non-conforming-scale .8)

(defun org-window-habit-rescale-assessment-value (value)
  (if (>= value 1.0) value
    (*  org-window-habit-non-conforming-scale value)))

(defun org-window-habit-lerp-color (color1 color2 proportion)
  (let ((r1 (string-to-number (substring color1 1 3) 16))
        (g1 (string-to-number (substring color1 3 5) 16))
        (b1 (string-to-number (substring color1 5 7) 16))
        (r2 (string-to-number (substring color2 1 3) 16))
        (g2 (string-to-number (substring color2 3 5) 16))
        (b2 (string-to-number (substring color2 5 7) 16)))
    (format "#%02x%02x%02x"
            (round (+ (* (- r2 r1) proportion) r1))
            (round (+ (* (- g2 g1) proportion) g1))
            (round (+ (* (- b2 b1) proportion) b1)))))

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
    (time-value duration-plist)
  (let* ((alignment-decoded (decode-time time-value))
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

(defun time-less-or-equal-p (time1 time2)
  (or (time-less-p time1 time2)
      (time-equal-p time1 time2)))

(defun time-greater-p (time1 time2)
  (time-less-p time2 time1))

(defun time-greater-or-equal-p (time1 time2)
  (time-less-or-equal-p time2 time1))

(defun org-window-habit-default-aggregation-fn (collection)
  (cl-loop for el in collection minimize (car el)))

(defclass org-window-habit ()
  ((window-specs :initarg :window-specs :initform nil)
   (assessment-interval :initarg :assessment-interval :initform '(:days 1))
   (reschedule-interval :initarg :reschedule-interval :initform '(:days 1))
   (reschedule-threshold :initarg :reschedule-threshold :initform 1.0)
   (done-times :initarg :done-times :initform nil)
   (assessment-decrement-plist :initarg :assessment-decrement-plist :initform nil)
   (max-repetitions-per-interval :initarg :max-repetitions-per-interval :initform 1)
   (aggregation-fn :initarg :aggregation-fn :initform 'org-window-habit-default-aggregation-fn)
   (graph-assessment-fn :initarg :graph-assessment-fn :initform nil)
   (start-time :initarg :start-time)))

(defclass org-window-habit-iterator ()
  ((window-spec :initarg :window-spec)
   (window :initarg :window)
   (start-index :initarg :start-index)
   (end-index :initarg :end-index)))

(defclass org-window-habit-window-spec ()
  ((duration-plist :initarg :duration :initform '(:days 1))
   (target-repetitions :initarg :repetitions :initform 1)
   (conforming-value :initarg :value :initform 1.0)
   (find-window :initarg :find-window :initform nil)
   (habit :initarg :habit)))

(defclass org-window-habit-assessment-window ()
  ((assessment-start-time :initarg :assessment-start-time)
   (assessment-end-time :initarg :assessment-end-time)
   (start-time :initarg :start-time)
   (end-time :initarg :end-time)))

(defun org-window-habit-get-window-where-time-in-last-assessment (spec time)
  (let* ((habit (oref spec habit))
         (assessment-plist
          (oref habit assessment-interval))
         (assessment-start
          (org-window-habit-normalize-time-to-duration
           time assessment-plist))
         (assessment-end
          (org-window-habit-keyed-duration-add-plist
           assessment-start
           assessment-plist))
         (window-start
          (org-window-habit-keyed-duration-add-plist
           assessment-end
           (org-window-habit-negate-plist (oref spec duration-plist)))))
    (make-instance
     'org-window-habit-assessment-window
     :assessment-start-time assessment-start
     :assessment-end-time assessment-end
     :start-time window-start
     :end-time assessment-end)))

(cl-defmethod org-window-habit-get-assessment-window
  ((spec org-window-habit-window-spec) time)
  (funcall (or (oref spec find-window)
               'org-window-habit-get-window-where-time-in-last-assessment)
           spec time))

(cl-defun org-window-habit-iterator-from-time (window-spec &optional time)
  (setq time (or time (current-time)))
  (let* ((iterator
          (make-instance 'org-window-habit-iterator
                         :window-spec window-spec
                         :window (org-window-habit-get-assessment-window window-spec time)
                         :start-index 0
                         :end-index 0)))
    (org-window-habit-adjust-iterator-indicies iterator)
    iterator))

(defun org-window-habit-create-instance-from-heading-at-point ()
  "Construct an org-window-habit instance from the current org entry."
  (save-excursion
    (let* ((done-times
            (cl-loop for state-change-info in (org-window-habit-parse-logbook)
                     if (member (nth 0 state-change-info) org-done-keywords)
                     collect (nth 2 state-change-info)))
           (done-times-vector (vconcat done-times))
           (assessment-interval
            (org-window-habit-string-duration-to-plist
             (org-entry-get nil "ASSESSMENT_INTERVAL") :default '(:days 1)))
           (reschedule-interval
            (org-window-habit-string-duration-to-plist
             (org-entry-get nil "RESCHEDULE_INTERVAL")))
           (max-repetitions-per-interval
            (string-to-number
             (or (org-entry-get nil "MAX_REPETITIONS_PER_INTERVAL" t) "1"))))
      (make-instance 'org-window-habit
                     :start-time nil
                     :window-specs (or
                                    (org-window-habit-create-specs)
                                    (org-window-habit-create-specs-from-perfect-okay))
                     :assessment-interval assessment-interval
                     :reschedule-interval reschedule-interval
                     :done-times done-times-vector
                     :max-repetitions-per-interval max-repetitions-per-interval))))

(defun org-window-habit-create-specs ()
  (let ((spec-text (org-entry-get nil "WINDOW_SPECS" t)))
    (when spec-text
      (cl-loop for args in (car (read-from-string spec-text))
               collect (apply 'make-instance 'org-window-habit-window-spec args)))))

(defun org-window-habit-create-specs-from-perfect-okay ()
  (let*
      ((window-length
        (org-window-habit-string-duration-to-plist
         (org-entry-get nil "WINDOW_DURATION" "1d") :default '(:days 1)))
       (repetitions-required
        (string-to-number
         (or (org-entry-get nil "REPETITIONS_REQUIRED" t) "1")))
       (okay-repetitions-required
        (string-to-number
         (or (org-entry-get nil "OKAY_REPETITIONS_REQUIRED" t) "1"))))
    (list
     (make-instance
      'org-window-habit-window-spec
      :duration window-length
      :repetitions repetitions-required
      :value 1.0)
     (make-instance
      'org-window-habit-window-spec
      :duration window-length
      :repetitions okay-repetitions-required
      :value .5))))

(cl-defmethod org-window-habit-earliest-completion ((habit org-window-habit))
  (with-slots (done-times) habit
    (let ((done-times-count (length done-times)))
      (when (> done-times-count 0)
        (aref done-times (- done-times-count 1))))))

(cl-defmethod initialize-instance :after ((habit org-window-habit) &rest _args)
  (when (null (oref habit assessment-interval))
    (oset habit assessment-interval (oref habit duration-plist)))
  (when (null (oref habit reschedule-interval))
    (oset habit reschedule-interval (oref habit assessment-interval)))
  (when (null (oref habit assessment-decrement-plist))
    (oset habit assessment-decrement-plist
          (org-window-habit-negate-plist (oref habit assessment-interval))))
  (when (null (oref habit start-time))
    (oset habit start-time
          (org-window-habit-normalize-time-to-duration
           (org-window-habit-earliest-completion habit)
           (oref habit assessment-interval))))
  (cl-loop for window-spec in (oref habit window-specs)
           do (oset window-spec habit habit)))

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
          :comparison 'time-less-p
          :start-index start-index)
         ;; We use start-time to compute the end index because the list is in
         ;; descending order
         (org-window-habit-find-array-forward
          done-times start-time
          :comparison 'time-less-or-equal-p
          :start-index end-index))
      (list
       ;; We use end-time and not start time because the array is in descending
       ;; order
       (org-window-habit-find-array-backward
        done-times end-time
        :comparison 'time-greater-p
        :start-index start-index)
       ;; We use start-time to compute the end index because the list is in
       ;; descending order
       (org-window-habit-find-array-backward
        done-times start-time
        :comparison 'time-greater-or-equal-p
        :start-index end-index)))))

(cl-defmethod org-window-habit-advance
  ((iterator org-window-habit-iterator) &key (amount nil))
  (with-slots (window window-spec) iterator
    (unless amount
      (setq amount (oref (oref window-spec habit) assessment-interval)))
    (let*
        ((new-start-time (org-window-habit-keyed-duration-add-plist
                          (oref window assessment-start-time)
                          amount))
         (window-moved-backward
          (time-less-p new-start-time (oref window assessment-start-time)))
         (new-window (org-window-habit-get-assessment-window window-spec new-start-time)))
      (oset iterator window new-window)
      (org-window-habit-adjust-iterator-indicies
       iterator (not window-moved-backward)))))

(cl-defmethod org-window-habit-effective-start ((iterator org-window-habit-iterator))
  (org-window-habit-time-max (oref (oref iterator window) start-time)
                             (oref (oref (oref iterator window-spec) habit) start-time)))

(cl-defmethod org-window-habit-adjust-iterator-indicies
  ((iterator org-window-habit-iterator)
   &optional window-moved-forward)
  (with-slots (window start-index end-index window-spec) iterator
      (cl-destructuring-bind (new-start-index new-end-index)
          (org-window-habit-get-completion-window-indices
           (oref window-spec habit)
           (oref window start-time) (oref window end-time)
           :start-index start-index
           :end-index end-index
           :reverse window-moved-forward)
        (oset iterator start-index new-start-index)
        (oset iterator end-index new-end-index))))

(cl-defmethod org-window-habit-conforming-ratio
  ((iterator org-window-habit-iterator) &rest args)
  (with-slots (window-spec window start-index) iterator
    (min
     1.0
     (/
      (apply 'org-window-habit-get-completion-count
             (oref window-spec habit)
             (oref window start-time)
             (oref window end-time)
             :start-index start-index
             args)
      (* (org-window-habit-actual-window-scale iterator)
         (oref window-spec target-repetitions))))))

(cl-defmethod org-window-habit-actual-window-scale
  ((iterator org-window-habit-iterator))
  (with-slots (window) iterator
    (org-window-habit-duration-proportion
     (oref window start-time) (oref window end-time)
     (org-window-habit-effective-start iterator))))

(cl-defmethod org-window-habit-get-conforming-value
  ((iterator org-window-habit-iterator) &rest args)
  (with-slots (window-spec) iterator
    (list (apply 'org-window-habit-conforming-ratio iterator args)
          (oref window-spec conforming-value))))

(cl-defmethod org-window-habit-build-graph ((habit org-window-habit) &optional now)
  (setq now (or now (current-time)))
  (with-slots
      (assessment-decrement-plist window-specs reschedule-interval
                                  max-repetitions-per-interval start-time aggregation-fn
                                  assessment-interval graph-assessment-fn)
      habit
    (unless graph-assessment-fn
      (setq graph-assessment-fn
            org-window-habit-graph-assessment-fn))
    (cl-destructuring-bind (actual-intervals actual-start-time)
        (cl-loop
         with target-start-time = (org-window-habit-normalize-time-to-duration
                                   now assessment-interval)
         for i from 0 to org-window-habit-preceding-intervals
         while (time-less-p start-time target-start-time)
         do
         (setq target-start-time
               (org-window-habit-keyed-duration-add-plist
                target-start-time
                assessment-decrement-plist))
         finally return (list i target-start-time))
      (nconc
       (cl-loop for i from 0 to (- org-window-habit-preceding-intervals actual-intervals)
                collect (list ?\s 'default))
       (cl-loop
        with iterators =
        (cl-loop for window-spec in window-specs
                 collect
                 (org-window-habit-iterator-from-time
                  window-spec actual-start-time))
        for current-assessment-start =
        (oref (oref (car iterators) window) assessment-start-time)
        for current-assessment-end =
        (oref (oref (car iterators) window) assessment-end-time)
        while (time-less-p current-assessment-end now)
        for conforming-values-no-comp =
        (cl-loop for iterator in iterators
                 collect (org-window-habit-get-conforming-value
                          iterator
                          :fill-completions-fn
                          (lambda (time actual-completions)
                            (if (time-equal-p current-assessment-start time)
                                0
                              actual-completions))))
        for assessment-value-no-comp = (or (funcall aggregation-fn conforming-values) 0.0)
        for conforming-values =
        (cl-loop for iterator in iterators
                 collect (org-window-habit-get-conforming-value iterator))
        for assessment-value = (funcall aggregation-fn conforming-values)
        collect
        (funcall
         graph-assessment-fn
         assessment-value-no-comp
         assessment-value
         (org-window-habit-get-completion-count
          habit current-assessment-start current-assessment-end)
         'past
         habit
         (oref (car iterators) window))
        into past-assessments
        do
        (cl-loop for iterator in iterators
                 do (org-window-habit-advance iterator))
        finally
        return
        (let*
            ((current-assessment-start (oref (oref (car iterators) window) assessment-start-time))
             (current-assessment-end (oref (oref (car iterators) window) assessment-end-time))
             (conforming-values (cl-loop for iterator in iterators collect
                                         (org-window-habit-get-conforming-value iterator)))
             (assessment-value (funcall aggregation-fn conforming-values))
             (with-completion-conforming-values
              (cl-loop for iterator in iterators
                       collect (org-window-habit-get-conforming-value
                                iterator
                                :fill-completions-fn
                                (lambda (time actual-completions)
                                  (if (time-equal-p current-assessment-start time)
                                      (+ actual-completions max-repetitions-per-interval)
                                    actual-completions)))))
             (with-completion-assessment-value
              (funcall aggregation-fn with-completion-conforming-values))
             (current-assessment
              (funcall
               graph-assessment-fn
               assessment-value
               with-completion-assessment-value
               (org-window-habit-get-completion-count
                habit current-assessment-start current-assessment-end)
               'present
               habit
               (oref (car iterators) window))))
          (nconc past-assessments (list current-assessment))))))))

(defun org-window-habit-make-graph-string (graph-info)
  (let ((graph (make-string (length graph-info) ?\s)))
    (cl-loop for (character face) in graph-info
             for index from 0
             do
             (progn
               (aset graph index character)
               (put-text-property index (1+ index) 'face face graph)))
    graph))

(defun org-window-habit-parse-todo-advice (orig &rest args)
  (if org-window-habit-mode
      (org-window-habit-create-instance-from-heading-at-point)
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

(defun org-window-habit-time-to-string (time)
  (format-time-string
   "%Y-%m-%d %H:%M"
   time))

(cl-defmethod org-window-habit-get-completion-count
  ((habit org-window-habit) start-time end-time &key (start-index 0)
   (fill-completions-fn (lambda (time actual-completions) actual-completions)))
  (cl-loop
   with next-start-index = start-index
   with interval-end-time = end-time
   for interval-start-time =
   ;; This is just a sanity check for the case where the interval does not
   ;; evenly divide the window. But you shouldn't do that anyway.
   (org-window-habit-time-max
    start-time
    (org-window-habit-keyed-duration-add-plist
     interval-end-time (oref habit assessment-decrement-plist)))
   for (start-index end-index) =
   (org-window-habit-get-completion-window-indices
    habit interval-start-time interval-end-time
    :start-index next-start-index
    :end-index next-start-index)
   for completions-within-interval =
   (min (oref habit max-repetitions-per-interval)
        (funcall
         fill-completions-fn
         interval-start-time
         (- end-index start-index)))
   sum completions-within-interval
   do (setq next-start-index end-index
            interval-end-time interval-start-time)
   while (time-less-p start-time interval-start-time)))

(cl-defmethod org-window-habit-get-next-required-interval
  ((habit org-window-habit) &optional now) (setq now (or now (current-time)))
  (with-slots
      (window-specs reschedule-interval reschedule-threshold assessment-interval
                    aggregation-fn done-times)
      habit
    (cl-loop
     with start-time =
     (org-window-habit-normalize-time-to-duration
      (org-window-habit-time-max
       now
       (org-window-habit-keyed-duration-add-plist (aref done-times 0)
                                                  reschedule-interval))
      assessment-interval)
     with iterators =
     (cl-loop for window-spec in window-specs
              collect
              (org-window-habit-iterator-from-time window-spec start-time))
     for current-assessment-start = (oref (oref (car iterators) window) assessment-start-time)
     for current-assessment-end = (oref (oref (car iterators) window) assessment-end-time)
     for conforming-values =
     (cl-loop for iterator in iterators
              collect (org-window-habit-get-conforming-value iterator))
     for assessment-value = (funcall aggregation-fn conforming-values)
     until (< assessment-value reschedule-threshold)
     do
     (cl-loop for iterator in iterators
              do (org-window-habit-advance iterator))
     finally return current-assessment-start)))

(cl-defun org-window-habit-default-graph-assessment-fn
    (without-completion-assessment-value
     with-completion-assessment-value
     completions-in-interval
     current-interval-time-type
     habit
     window)
  (let* ((with-completion-color
          (org-window-habit-lerp-color
           org-window-habit-not-conforming-color
           org-window-habit-conforming-color
           (org-window-habit-rescale-assessment-value
            with-completion-assessment-value)))
         (without-completion-color
          (org-window-habit-lerp-color
           org-window-habit-not-conforming-color
           org-window-habit-conforming-color
           (org-window-habit-rescale-assessment-value
            without-completion-assessment-value)))
         (completion-today-matters
          (< without-completion-assessment-value with-completion-assessment-value))
         (interval-is-present (eq current-interval-time-type 'present))
         (completion-expected-today
          (and interval-is-present
                 (time-less-p
                  (org-window-habit-get-next-required-interval habit)
                  (oref window assessment-end-time))))
         (bg-color
          (if completion-expected-today
              without-completion-color
              with-completion-color))
         (fg-color
          (cond
           (completion-expected-today
            with-completion-color)
           (completion-today-matters
            org-window-habit-required-completion-foreground-color)
           (t org-window-habit-non-required-completion-foreground-color)))
         (face (org-window-habit-create-face bg-color fg-color))
         (character
          (cond
           ((> completions-in-interval 0) org-habit-completed-glyph)
           (completion-expected-today
            org-window-habit-completion-needed-today-glyph)
           (t ?\s))))
    (list character face)))

(defun org-window-habit-auto-repeat (&rest args)
  (interactive)
  (let* ((required-interval-start
          (org-window-habit-get-next-required-interval
           (org-window-habit-create-instance-from-heading-at-point)))
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
