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
  "Выполняет разбор файла, с данными импортированными из ячеек exel в которых:
- первая колонка - обозначение и размерность;
- вторая колонка - обозначение сигнала.
"
  (mapcar #'(lambda (el) (mapcar #'string-to-float-string el))
	  (exel-to-underlining (exel-pname-dimension (read-exel fname)))))

(export 'read-from-exel-nds)

(defmacro add-row (p-name p-dim p-signal lst place)
  `(push (append (list ,p-name ,p-dim ,p-signal) ,lst) ,place))

(export 'add-row)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

