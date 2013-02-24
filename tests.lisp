;; _____________________________________________________________
;; bpint

(defmacro bprint (var)
  `(subseq (with-output-to-string (*standard-output*)  (pprint ,var)) 1))

(setf x (format nil "~A~%~A~%~A" 54 4 3))

(bprint x)

(print (macroexpand-1 '(bprint x)))

(with-output-to-string (*standard-output*) (pprint x))

(print (read))

(print x)

;; _____________________________________________________________
;; multiple-value-bind

(defun show-value (key hash-table)
  (multiple-value-bind (value present) (gethash key hash-table)
    (if present
        (format nil "Значение ~a присутствует в таблице." value)
        (format nil "Значение равно ~a, поскольку ключ не найден." value))))

(defparameter *h* (make-hash-table))

(gethash 'foo *h*)

(setf (gethash 'foo *h*) 'quux)

(gethash 'foo *h*)

(setf (gethash 'bar *h*) nil)

(show-value 'foo *h*)

(show-value 'bar *h*)

(show-value 'baz *h*)

;; _____________________________________________________________
;; error

(define-condition to-many-files-error (error)
  ((text :initarg :text :reader text)))

(defun err ()
  (error 'to-many-files-error))

(setq x "ошибка")

(handler-case (err)
  (to-many-files-error () (format t "~A" x)))

;; Пример функции error

(setq a 'fred)

(if (numberp a) (1+ a) (error "~S is not a number." A))

;; Так как а не number, происходит переброс в отладчик и выводится сообщение
;; FRED is not a number.
