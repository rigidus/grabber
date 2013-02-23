(require 'cl-fad)
(require 'split-sequence)
(require 'alexandria)
(require 'cl-ppcre)
(setf ppcre:*use-bmh-matchers* nil)
(require 'drakma)
(require 'hunchentoot)
(require 'cl-json)
(require 'babel)
(require 'ironclad)
(load #P"puri-unicode.lisp")


(defparameter *user-agent* "Mozilla/5.0 (X11; U; Linux i686; ru; rv:1.9.2.13) Gecko/20101206 Ubuntu/10.04 (lucid) Firefox/3.6.13")
;; задаётся клиент (браузер)
(defparameter *strategy* nil)
;; *strategy* присваивается значение nil
(defparameter *hashes* (make-hash-table :test #'equal))
;; *hashes* присваивается хеш таблица с ключом #'equal (функция сравнения)

(defconstant +many-files+ 999999)
;; глобальной константе +many-files+ присваивается значение 999999 (много файлов)

(define-condition to-many-files-error (error)
  ((text :initarg :text :reader text)))
;; define-condition - макрос, определяющий ошибки
;; to-many-files-error - ошибка, слишком много файлов (больше, чем 9999999)

(define-condition non-200-status-error (error)
  ((text :initarg :text :reader text)))
;; non-200-status-error - ошибка, сервер не открыл страницу

(define-condition non-download-link-error (error)
  ((text :initarg :text :reader text)))
;; non-download-link-error - ошибка, файл не загружается, т. к. есть пароль

(define-condition filtered-by-name-error (error)
  ((text :initarg :text :reader text)))
;; filtered-by-name-error - ошибка, файл не отсортирован по имени

(define-condition file-exists-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (format stream "File '~A' exists!" (text condition)))))
;; file-exists-error - ошибка, файл уже существует
;; вывод сообщения об этом

(define-condition extended-char-error (error)
  ((text :initarg :text :reader text))
  (:report
   (lambda (condition stream)
     (format stream "Extended-char in file content"
             (text condition)))))
;; extended-char-error - ошибка, файл содержит нестандартные символы
;; вывод сообщения об этом

(define-condition unknown-content-type-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (format stream "Unknown type of file content in function save data" (text condition)))))
;; unknown-content-type-error - тип данных не известен
;; вывод сообщения об этом

(defmacro bprint (var)
  `(subseq (with-output-to-string (*standard-output*)  (pprint ,var)) 1))
;; bprint - макрос, который позволяет напечатаь строку без перехода на новую строку

(defmacro err (var)
  `(error (format nil "ERR:[~A]" (bprint ,var))))

(defmacro make-sanitize (deprecated)
  `(defun sanitize (string)
     (let ((ret))
       (loop :for x :across string :do
          (or
           ,@(loop :for character :across deprecated :collect
                `(equal x ,character))
           (push x ret)))
       (coerce (reverse ret) 'string))))

(make-sanitize "()[]{}'\"!%\/&")

(defun get-link (body)
  (let ((start (search "http://rghost.net/download/" body)))
    (if (or (null start))
        (error 'non-download-link-error :text (format nil "password detected"))
        (subseq body start (search "\"" body :start2 start)))))

(defun get-page (number fun)
  (multiple-value-bind (data status-code)
      (drakma:http-request (format nil "http://rghost.net/~A" number))
    (if (not (equal 200 status-code))
        (error 'non-200-status-error :text (format nil "~A : ~A" number status-code))
        (funcall fun data))))

(defun rghost-filter-by-name (name)
  (or
   (ppcre:scan "(?i)(.*)sa(.*)\\.jpg" name)))

(defun download (number filter)
  (let* ((link  (get-page number #'get-link))
         (name (sanitize (format nil "~A" (car (last (puri:uri-parsed-path (puri:parse-uri link))))))))
    (when (funcall filter name)
      (error 'filtered-by-name-error :text name))
    (multiple-value-bind (data status-code)
        (drakma:http-request (get-page number #'get-link))
      (when (not (equal 200 status-code))
        (error 'non-200-status-error :text (format nil "~A : ~A" number status-code)))
      (values
       name
       data))))

(defun read-new-name ()
  (format t "Enter a new file name (in quotes): ")
  (list (safe-name (car (multiple-value-list (eval (read)))))))

(defun safe-name(name)
  (flet ((find-rename-num (name)
           (do ((i 0))
               ((> i +many-files+) (error 'to-many-files-error))
             (incf i)
             (if (not (probe-file (pathname (format nil "./files/~A(~A)" name i))))
                 (let ((old-name name))
                   (setf name (format nil "~A(~A)" name i))
                   (format t "Warn: Existing file '~A' renamed to ~A~%" old-name name)
                   (return-from find-rename-num name))))))
    (restart-case
        (if (probe-file (pathname (format nil "./files/~A" name)))
            (error 'file-exists-error :text name))
      (overwrite ()
        :report "Overwrite the file. Old file will be lost."
        (format t "Warn: Existing file '~A' overwrited~%" name)
        name)
      (overwrite-all ()
        :report "Overwrite all files."
        (progn
          (setf *strategy* 'overwrite)
          (safe-name name)))
      (skip ()
        :report "Skip to a new file. The old file will be saved."
        (progn
          (format t "Warn: Existing file '~A' skipped~%" name)
          (return-from safe-name name)))
      (skip-all ()
        :report "Skip all new file."
        (progn
          (setf *strategy* 'skip)
          (safe-name name)
          (return-from safe-name name)))
      (enter-name (new-name)
        :report "Enter a name for the new file"
        :interactive read-new-name (setf name new-name))
      (auto-rename ()
        :report "Add sequence number to the file name"
        (setf name (find-rename-num name)))
      (auto-rename-all ()
        :report "Add sequence number to all files"
        (progn
          (setf *strategy* 'rename)
          (setf name (find-rename-num name)))))
    name))

(defun save (number)
  (multiple-value-bind (name data)
      (download number #'rghost-filter-by-name)
    (let* ((new-name (safe-name name))
           (stream-type (cadr (type-of data)))
           (new-data
            (cond ((equal stream-type 'character)
                     (handler-case (ironclad:ascii-string-to-byte-array data)
                       (simple-error () (format t "~A: ~A"
                                                (error 'extended-char-error :text name)
                                                name))))
                  ;; (error 'extended-char-error :text name)))
                  ((equal stream-type '(unsigned-byte 8)) data)
                  (t (error 'unknown-content-type-error :text stream-type))))
           (hash-str (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :md5 new-data))))
      (format t "~A:~A: ~A~%" number hash-str new-name)
      (with-open-file (stream (pathname (format nil "./files/~A" new-name))
                              :element-type stream-type
                              :direction :output
                              :if-exists :supersede)
        (write-sequence data stream)))))

(defun rghost(start end)
  (do ((i start))
      ((< i end))
    (handler-bind ((file-exists-error
                    #'(lambda (se)
                        (declare (ignore se))
                        (cond ((equal *strategy* 'skip)       (invoke-restart 'skip))
                              ((equal *strategy* 'overwrite)  (invoke-restart 'overwrite))
                              ((equal *strategy* 'rename)     (invoke-restart 'auto-rename))
                              (t nil)))))
      (handler-case
          (save i)
        (non-200-status-error (se)    (format t "~A: ~A~%" (bprint se) (text se)))
        (non-download-link-error (se) (format t "~A: ~A: ~A~%" (bprint se) i (text se)))
        (filtered-by-name-error (se)  (format t "~A: ~A: ~A~%" (bprint se) i (text se)))
        (extended-char-error (se)  (format t "~A: ~A: ~A~%" (bprint se) i (text se)))
        ))
    (decf i))
  (print 'fin))

(rghost 43546555 43450000)
