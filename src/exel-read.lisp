(defpackage exel-read 
  (:use :cl))

(in-package :exel-read)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split-exel-line (line) (str:split #\Tab (string-trim '(#\Space #\Return #\Newline) line)))

(export 'read-exel )
(defun read-exel (file-name)
"@b(Описание:) функция @b(read-exel) считывает данные из файла с именем @b(file-name), 
который был получен путем операций копирования и вставки из файла EXEL в текстовый файл 
с кодировкой utf8 (можно (нужно) использовать emacs в качестве редактора).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (read-exel \"~/quicklisp/local-projects/mnas/exel-read/tests/test.txt\")
 =>
 '(
   (\"Строки спецификации\" \"\" \"\" \"\" \"\" \"\" \"\" \"\" \"\" \"\")
   (\"25.08.2020 13:59:33 [IT-Enterprise.TKSP.LOS055]\" \"\" \"\" \"\" \"\" \"\" \"\" \"\" \"\" \"\")
   ...
   (\"+\" \" \" \"П\" \"Дт\" \"В5Г80011035 Проставка\" \"А3\" \"3A\" \"19\" \"1.000000\" \"шт\"))
@end(code)
"
  (let ((rez nil))
    (with-open-file (stream file-name :direction :input :external-format :utf8)
      (do ((line (read-line stream nil) (read-line stream nil)))
	  ((null line) (reverse rez))
	(push (split-exel-line line) rez)))))

(export 'split-string-name-dim )
(defun split-string-name-dim (separator str)
"@b(Описание:) split-string-name-dim "
  (let ((rez (str:split separator str)))
    (cond
      ((= 1 (length rez)) (append rez '("-")))
      ((= 2 (length rez)) rez)
      ((< 2 (length rez)) (list (first rez) (second rez))))))


(export 'exel-pname-dimension )
(defun exel-pname-dimension (lst)
"@b(Описание:) exel-pname-dimension "
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

(export 'get-row )
(defun get-row (key data &key (col-ignore 3))
"@b(Описание:) get-row "
  (let ((rez   (assoc key data :test #'equal)))
    (dotimes (i col-ignore rez)
      (pop rez))))

(export 'string-to-float-string )
(defun string-to-float-string (str)
"@b(Описание:) функция @b(string-to-float-string) выполняет преобразование строки в число.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (string-to-float-string \"15.5\")         => 15.5
 (string-to-float-string \"15,5\")         => 15.5
 (string-to-float-string \"15,5d-1\")      => 1.55d0 
 (string-to-float-string \"15\")           => 15.0
 (string-to-float-string \"\")             => \"-\"
 (string-to-float-string \"Abra-Codabra\") => \"Abra-Codabra\"
@end(code)
"
  (let ((str-clean
	 (substitute #\- #\: (string-trim '(#\Space) (substitute #\. #\, str)))))
    (if (string= "" str-clean)
	"-"
	(multiple-value-bind (rez num) (read-from-string str-clean)
	  (cond
	    ((and (= (length str-clean) num) (numberp rez)) (float rez))
	    (t str))))))

(export 'read-from-exel-nds )
(defun read-from-exel-nds (fname)
"@b(Описание:) read-from-exel-nds выполняет разбор файла, с данными импортированными из ячеек exel в которых:
- первая колонка - обозначение и размерность;
- вторая колонка - обозначение сигнала.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (read-from-exel-nds \"~/quicklisp/local-projects/mnas/exel-read/tests/test_2.txt\")
 '((\"PВ_КДТ1\" \"-\" 10.0 20.0 30.0 40.0 50.0 60.0)
   (\"p2\" \"-\" 101.0 102.0 103.0 104.0 105.0 103.0))
@end(code)
"
  (mapcar #'(lambda (el) (mapcar #'string-to-float-string el))
	  (exel-to-underlining (exel-pname-dimension (read-exel fname)))))

(export 'add-row )
(defmacro add-row (p-name p-dim p-signal lst place)
"@b(Описание:) add-row "
  `(push (append (list ,p-name ,p-dim ,p-signal) ,lst) ,place))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'r-exel )
(defun r-exel (&key (title "Выберите файл Εχελ, содержащий сторки спецификации, экспортированной из IT-Предприятие")
		 (skip-lines-number 3 ))
"@b(Описание:) функция @b(r-exel) выполняет чтение файла xlsx, указываемого в диалоговом окне.
Предназначена для импортирования содержимого спецификаций, выгружаемых из системы IT-Предприятие.

 @b(Возвращает:) Состав файла в виде списка списков.

 @b(Переменые:)
@begin(list)
 @item(title - заголовок окна для диалога открытия файла;)
 @item(skip-lines-number - количество пропускаемых строк.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (r-exel)
 =>
'(
  (\"+\" \"\" \"П\" \"Дк\" \"В5Г80011857СБ Форсунка Сборочный чертеж\" \"А1\" \"\" \"\" NIL \"шт\" NIL \"2017-02-14\" \"9999-12-31\" \"ПВ5Г80011857СБ0\" \"ПВ5Г80011857000\" \"\" \"\" \"\" \"\" NIL \"\" \"\" NIL \"\" \"Рябов Д.В.\" NIL \"Рябов Д.В.\" NIL \"\" NIL NIL \"\")
  (\"\" \"\" \"П\" \"Дк\" \"В5Г80011857 ФО Формуляр\" \"А4\" \"\" \"\" NIL \"шт\" NIL \"2017-02-14\" \"9999-12-31\" \"ПВ5Г80011857ФО0\" \"ПВ5Г80011857000\" \"\" \"\" \"\" \"\" NIL \"Форма 424Т\" \"\" NIL \"\" \"Рябов Д.В.\" NIL \"Рябов Д.В.\" NIL \"\" NIL NIL \"\")
  ...
  (\"\" \"+\" \"П\" \"Мт\" \"Проволока 1,0-ТС-12Х18Н10Т ГОСТ 18143-72\" \"\" \"3A\" \"49\" NIL \"кг(м)\" NIL \"2017-02-14\" \"9999-12-31\" \"М290510V0002000\" \"ПВ5Г80011857000\" \"\" \"\" \"\" \"\" NIL \"м Повторно не применять\" \"\" NIL \"\" \"Рябов Д.В.\" NIL \"Рябов Д.В.\" NIL \"005\" NIL NIL \"\")
  )
@end(code)
"
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
