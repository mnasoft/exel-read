(defpackage exel-read
  (:use :cl))
(in-package :exel-read)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split-exel-line (line) (str:split #\Tab (string-trim '(#\Space #\Return #\Newline) line)))

(defun read-exel (file-name)
  (let ((rez nil))
    (with-open-file
	(stream
	 file-name
	 :direction :input
	 :external-format :utf8)
      (do ((line (read-line stream nil) (read-line stream nil)))
	  ((null line) (reverse rez))
	(push (split-exel-line line) rez)))))

(export 'read-exel)

(defun split-string-name-dim (separator str)
  (let ((rez (str:split separator str)))
    (cond
      ((= 1 (length rez)) (append rez '("-")))
      ((= 2 (length rez)) rez)
      ((< 2 (length rez)) (list (first rez) (second rez))))))

(export 'split-string-name-dim)

(defun exel-pname-dimension (lst)
  (mapcar #'(lambda (el) (append (split-string-name-dim ", "(pop el)) el )) lst))

(export 'exel-pname-dimension)

;;;;
(defun to-underliningp (ch)
  (declare (type character ch))
  (when (member ch '(#\Space #\Return #\Comma #\.)) t))

(defun replace-to-underlining (str)
  (declare (type string str))
  (substitute-if #\_ #'to-underliningp str))

(defun exel-to-underlining (lst)
  (mapcar #'(lambda (el) (append (list (replace-to-underlining (pop el))) el )) lst))
;;;;

(defun get-row (key data &key (col-ignore 3))
  (let ((rez   (assoc key data :test #'equal)))
    (dotimes (i col-ignore rez)
      (pop rez))))

(export 'get-row)

(defun string-to-float-string (str)
  (let ((str-clean
	 (substitute #\- #\: (string-trim '(#\Space) (substitute #\. #\, str)))))
    (if (string= "" str-clean)
	"-"
	(multiple-value-bind (rez num) (read-from-string str-clean)
	  (cond
	    ((and (= (length str-clean) num) (numberp rez)) (float rez))
	    (t str))))))

(export 'string-to-float-string)

(defun read-from-exel-nds (fname)
  (mapcar #'(lambda (el) (mapcar #'string-to-float-string el))
	  (exel-to-underlining (exel-pname-dimension (read-exel fname)))))

(export 'read-from-exel-nds)

(defmacro add-row (p-name p-dim p-signal lst place)
  `(push (append (list ,p-name ,p-dim ,p-signal) ,lst) ,place))

(export 'add-row)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-vlisp-apps (&key (os *standard-output*))
  (vlisp:load-vlisp-file "./bin/Axis.VLX"      :os os)
  (vlisp:load-vlisp-file "./bin/dim_style.VLX" :os os)
  (vlisp:load-vlisp-file "./bin/lines.VLX"     :os os))

(defun draw-all-axis (&key (os *standard-output*))
  (vlisp:dr-axis '(-33.4915 266.254 0.0) '(-33.4915 346.254 0.0) 10000.0 18000.0 0 "nКМ" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-133.492 6.25428 0.0) '(-133.492 141.254 0.0) 0.0 120.0 0 "PВ_КДТ1" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-143.492 6.25428 0.0) '(-143.492 141.254 0.0) 0.0 120.0 0 "PВ_КВ2" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-153.492 6.25428 0.0) '(-153.492 141.254 0.0) 0.0 2000.0 0 "PВ_КГТ3" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-53.4915 206.254 0.0) '(-53.4915 256.254 0.0) 0.0 100.0 0 "tНВ" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-43.4915 -3.74572 0.0) '(-43.4915 196.254 0.0) 0.0 500.0 0 "tВ_ВО_ВХ" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-53.4915 -3.74572 0.0) '(-53.4915 196.254 0.0) 0.0 500.0 0 "tКМ_ВХ" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-63.4915 -3.74572 0.0) '(-63.4915 196.254 0.0) 0.0 500.0 0 "tКМ_ВЫХ" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-33.4915 206.254 0.0) '(-33.4915 256.254 0.0) 0.0 100.0 0 "tКМ_ПОДШ_1" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-43.4915 206.254 0.0) '(-43.4915 256.254 0.0) 0.0 100.0 0 "tКМ_ПОДШ_2" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-73.4915 -3.74572 0.0) '(-73.4915 196.254 0.0) 0.0 500.0 0 "tЭ_КМ_ОБ" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-71.8007 266.254 0.0) '(-71.8007 346.254 0.0) 0.0 16.0 0 "VКМ_Г" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-81.8007 266.254 0.0) '(-81.8007 346.254 0.0) 0.0 16.0 0 "VКМ_О" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-91.8007 266.254 0.0) '(-91.8007 316.254 0.0) 0.0 100.0 0 "KSAM" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-33.4915 -3.74572 0.0) '(-33.4915 196.254 0.0) 0.0 500.0 0 "tВ_КС_ВЫХ" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-83.4915 -3.74572 0.0) '(-83.4915 196.254 0.0) 0.0 500.0 0 "t02БАЛ" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-233.492 6.25428 0.0) '(-233.492 141.254 0.0) 0.0 120.0 0 "ΔPКДТ1-P2" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-243.492 6.25428 0.0) '(-243.492 141.254 0.0) 0.0 120.0 0 "ΔPКВ2-P2" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-253.492 6.25428 0.0) '(-253.492 141.254 0.0) 0.0 120.0 0 "ΔPКГТ3-P2" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-222.064 156.254 0.0) '(-222.064 256.254 0.0) 0.0 1000.0 0 "GВ_ФОРС" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-212.064 156.254 0.0) '(-212.064 256.254 0.0) 0.0 1000.0 0 "GВ_КМ" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-183.492 6.25428 0.0) '(-183.492 141.254 0.0) 0.0 120.0 0 "PВ_КДТ1-P2" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-193.492 6.25428 0.0) '(-193.492 141.254 0.0) 0.0 120.0 0 "PВ_КВ2-P2" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(-203.492 6.25428 0.0) '(-203.492 141.254 0.0) 0.0 120.0 0 "PВ_КГТ3-P2" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(300.0 -53.7457 0.0) '(525.0 -53.7457 0.0) 250.0 700.0 0 "t04" :caption "" :dimension "" :os os)
  (vlisp:dr-axis '(600.0 -53.7457 0.0) '(825.0 -53.7457 0.0) 200.0 1700.0 0 "p2" :caption "" :dimension "" :os os))

(defun make-graph (x-param y-params data &key (os *standard-output*) )
  (vlisp:axis-draw-spline-set nil :os os )
  (vlisp:axis-draw-multiple-graphs-by-axis-names
   x-param (get-row x-param data)
   y-params (mapcar #'(lambda (el) (get-row el data)) y-params) :os os))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((data (read-from-exel-nds #p"~/quicklisp/local-projects/clisp/exel-read/src/dt.txt")))
  (progn
    (add-row "PВ_КДТ1-P2" "кПа" "-" (mapcar #'- (get-row  "PВ_КДТ1" data) (get-row  "p2" data)) data)
    (add-row "PВ_КВ2-P2"  "кПа" "-" (mapcar #'- (get-row  "PВ_КВ2" data)  (get-row  "p2" data)) data)
    (add-row "PВ_КГТ3-P2" "кПа" "-" (mapcar #'- (get-row  "PВ_КГТ3" data) (get-row  "p2" data)) data)
    (add-row "tКМ_ВХ"     "°C"  "-" (mapcar #'math:averange (get-row  "tКМ_ВХ_1"  data) (get-row  "tКМ_ВХ_2"  data)) data)
    (add-row "tКМ_ВЫХ"    "°C"  "-" (mapcar #'math:averange (get-row  "tКМ_ВЫХ_1" data) (get-row  "tКМ_ВЫХ_2" data)) data))
  (defparameter *dt* data)
  (with-open-file (os "~/dt.lsp" :direction :output :if-exists :supersede :external-format :utf8)
      (progn (load-vlisp-apps :os os) (draw-all-axis :os os))
;;;; p2-dt
    (make-graph "p2" '("nКМ" "KSAM"	    
		       "GВ_КМ"
		       "ΔPКВ2-P2" "ΔPКГТ3-P2" "PВ_КВ2-P2" "PВ_КГТ3-P2")
		data :os os)

;;;; t04-dt
    (make-graph "t04" '("nКМ" "KSAM" "VКМ_О" "VКМ_Г"
			"tКМ_ПОДШ_1" "tКМ_ПОДШ_2" "tНВ"
			"tКМ_ВХ"  "tКМ_ВЫХ" "tЭ_КМ_ОБ" "tВ_КС_ВЫХ" "tВ_ВО_ВХ" "t02БАЛ") 
		data :os os)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((data (read-from-exel-nds #p"~/quicklisp/local-projects/clisp/exel-read/src/gt.txt")))
  (progn
    (add-row "PВ_КДТ1-P2" "кПа" "-" (mapcar #'- (get-row  "PВ_КДТ1" data) (get-row  "p2" data)) data)
    (add-row "PВ_КВ2-P2"  "кПа" "-" (mapcar #'- (get-row  "PВ_КВ2" data)  (get-row  "p2" data)) data)
    (add-row "PВ_КГТ3-P2" "кПа" "-" (mapcar #'- (get-row  "PВ_КГТ3" data) (get-row  "p2" data)) data)
    (add-row "tКМ_ВХ"     "°C"  "-" (mapcar #'math:averange (get-row  "tКМ_ВХ_1"  data) (get-row  "tКМ_ВХ_2"  data)) data)
    (add-row "tКМ_ВЫХ"    "°C"  "-" (mapcar #'math:averange (get-row  "tКМ_ВЫХ_1" data) (get-row  "tКМ_ВЫХ_2" data)) data))
  (defparameter *gt* data)
  (with-open-file (os "~/gt.lsp" :direction :output :if-exists :supersede :external-format :utf8)
    (progn (load-vlisp-apps :os os) (draw-all-axis :os os))
;;;; p2-gt
    (make-graph "p2" '("nКМ" "KSAM"	    
		       "GВ_КМ"
		       "ΔPКДТ1-P2" "ΔPКВ2-P2")
		data :os os)
;;;; t04-gt
    (make-graph "t04" '("nКМ" "KSAM" "VКМ_О" "VКМ_Г"
			"tКМ_ПОДШ_1" "tКМ_ПОДШ_2" "tНВ"
			"tКМ_ВХ"  "tКМ_ВЫХ" "tЭ_КМ_ОБ" "tВ_КС_ВЫХ" "tВ_ВО_ВХ" "t02БАЛ")
		data :os os)))
