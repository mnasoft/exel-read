(defpackage exel-read 
  (:use :cl))

(in-package :exel-read)

(annot:enable-annot-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split-exel-line (line) (str:split #\Tab (string-trim '(#\Space #\Return #\Newline) line)))

@export
@annot.doc:doc
"@b(Описание:) read-exel "
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

@export
@annot.doc:doc
"@b(Описание:) split-string-name-dim "
(defun split-string-name-dim (separator str)
  (let ((rez (str:split separator str)))
    (cond
      ((= 1 (length rez)) (append rez '("-")))
      ((= 2 (length rez)) rez)
      ((< 2 (length rez)) (list (first rez) (second rez))))))


@export
@annot.doc:doc
"@b(Описание:) exel-pname-dimension "
(defun exel-pname-dimension (lst)
  (mapcar #'(lambda (el) (append (split-string-name-dim ", "(pop el)) el )) lst))

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

@export
@annot.doc:doc
"@b(Описание:) get-row "
(defun get-row (key data &key (col-ignore 3))
  (let ((rez   (assoc key data :test #'equal)))
    (dotimes (i col-ignore rez)
      (pop rez))))

@export
@annot.doc:doc
"@b(Описание:) string-to-float-string "
(defun string-to-float-string (str)
  (let ((str-clean
	 (substitute #\- #\: (string-trim '(#\Space) (substitute #\. #\, str)))))
    (if (string= "" str-clean)
	"-"
	(multiple-value-bind (rez num) (read-from-string str-clean)
	  (cond
	    ((and (= (length str-clean) num) (numberp rez)) (float rez))
	    (t str))))))


@export
@annot.doc:doc
"@b(Описание:) read-from-exel-nds выполняет разбор файла, 
с данными импортированными из ячеек exel в которых:
- первая колонка - обозначение и размерность;
- вторая колонка - обозначение сигнала.
"
(defun read-from-exel-nds (fname)

  (mapcar #'(lambda (el) (mapcar #'string-to-float-string el))
	  (exel-to-underlining (exel-pname-dimension (read-exel fname)))))

@export
@annot.doc:doc
"@b(Описание:) add-row "
(defmacro add-row (p-name p-dim p-signal lst place)
  `(push (append (list ,p-name ,p-dim ,p-signal) ,lst) ,place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"@b(Описание:) r-exel выполняет чтение файла xlsx.
@b(Переменые:)
@begin(list)
 @item(skip-lines-number - количество пропускаемых строк;)
 @item(title - заголовок окна диалога для откріваемого файла. )
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (r-exel)
@end(code)
"
(defun r-exel (&key (title "Выберите файл Εχελ, содержащий сторки спецификации, экспортированной из IT-Предприятие")
		 (skip-lines-number 3 )) 
  (let* ((exel-lines (xlsx:as-matrix
		      (mnas-xlsx:read-sheet
		       (mnas-file-dialog:get-open-file
			:filetypes '(("Файлы Εχελ" "*.xlsx"))
			:title  title ))))
	 (sp-lines (lst-arr:array2d->list-list-by-row exel-lines)))
    
    (block skip-lines-and-replace-nil-to-empty-string
      (mapcar #'(lambda (el1)
		  (mapcar
		   #'(lambda (el)
		       (cond
			 ((null el) "")
			 ((stringp el) (mnas-string:replace-all el "
" " "))))
		   el1))
	      (nthcdr skip-lines-number sp-lines)))))
