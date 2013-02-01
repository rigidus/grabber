(require 'cl-fad)
(require 'split-sequence)
(require 'alexandria)
(require 'cl-ppcre)
(setf ppcre:*use-bmh-matchers* nil)
(require 'drakma)
(require 'hunchentoot)
(require 'cl-json)
(require 'babel)
(load #P"puri-unicode.lisp")


(defparameter *user-agent* "Mozilla/5.0 (X11; U; Linux i686; ru; rv:1.9.2.13) Gecko/20101206 Ubuntu/10.04 (lucid) Firefox/3.6.13")
(defparameter *strategy* nil)

(defconstant +many-files+ 3)
(define-condition to-many-files-error (error)
  ((text :initarg :text :reader text)))

(define-condition non-200-status-error (error)
    ((text :initarg :text :reader text)))

(define-condition non-download-link-error (error)
  ((text :initarg :text :reader text)))

(define-condition file-exists-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (format stream "File '~A' exists!" (text condition)))))



(defmacro bprint (var)
  `(subseq (with-output-to-string (*standard-output*)  (pprint ,var)) 1))

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

(make-sanitize "()[]{}'\"!%\/")



(defun get-link (body)
  (let ((start (search "http://rghost.net/download/" body)))
    (if (or (null start))
        (error 'non-download-link-error :text (format nil "password detected"))
        (subseq body start (search "\"" body :start2 start)))))

(defun get-page (number fun)
  (multiple-value-bind (body-or-stream status-code headers uri stream-out must-close reason-phrase)
      (drakma:http-request (format nil "http://rghost.net/~A" number))
    (if (not (equal 200 status-code))
        (error 'non-200-status-error :text (format nil "~A : ~A" number status-code))
        (funcall fun body-or-stream))))



(defun download (number)
  (let* ((link  (get-page number #'get-link))
         (fname (sanitize (car (last (puri:uri-parsed-path (puri:parse-uri link)))))))
    (multiple-value-bind (body-or-stream status-code headers uri stream-out must-close reason-phrase)
        (drakma:http-request (get-page number #'get-link))
      (values
       fname
       body-or-stream))))

(defun read-new-fname ()
  (format t "Enter a new file name (in quotes): ")
  (list (safe-fname (car (multiple-value-list (eval (read)))))))

(defun safe-fname(fname)
  (flet ((find-rename-num (fname)
           (do ((i 0))
               ((> i +many-files+) (error 'to-many-files-error))
             (incf i)
             (if (not (probe-file (pathname (format nil "./files/~A(~A)" fname i))))
                 (let ((old-fname fname))
                   (setf fname (format nil "~A(~A)" fname i))
                   (format t "Warn: Existing file '~A' renamed to ~A~%" old-fname fname)
                   (return-from find-rename-num fname))))))
    (restart-case
        (if (probe-file (pathname (format nil "./files/~A" fname)))
            (error 'file-exists-error :text fname))
      (overwrite ()
        :report "Overwrite the file. Old file will be lost."
        (format t "Warn: Existing file '~A' overwrited~%" fname)
        fname)
      (overwrite-all ()
        :report "Overwrite all files."
        (progn
          (setf *strategy* 'overwrite)
          (safe-fname fname)))
      (skip ()
        :report "Skip to a new file. The old file will be saved."
        (progn
          (format t "Warn: Existing file '~A' skipped~%" fname)
          (return-from safe-fname fname)))
    (skip-all ()
      :report "Skip all new file."
      (progn
        (setf *strategy* 'skip)
        (safe-fname fname)
        (return-from safe-fname fname)))
    (enter-name (new-fname)
      :report "Enter a name for the new file"
      :interactive read-new-fname (setf fname new-fname))
    (auto-rename ()
      :report "Add sequence number to the file name"
      (setf fname (find-rename-num fname)))
    (auto-rename-all ()
      :report "Add sequence number to all files"
      (progn
        (setf *strategy* 'rename)
        ;; (safe-fname fname)
        (setf fname (find-rename-num fname)))))
  fname))

(defun save (number)
  (multiple-value-bind (fname fdata)
      (download number)
    (setf fname (safe-fname fname))
    (format t "~A: ~A~%" number fname)
    (let ((stream-type (cadr (type-of fdata))))
      (with-open-file (stream (pathname (format nil "./files/~A" fname))
                              :element-type stream-type
                              :direction :output
                              :if-exists :supersede)
        (format t "Write to '~A' ~%" fname)
        (write-sequence fdata stream)))))


(defun rghost(start end)
  (format t "=RGHOST=~%")
  (do ((i start))
      ((< i end))
    (handler-bind ((file-exists-error
                    #'(lambda (se)
                        (cond ((equal *strategy* 'skip)       (invoke-restart 'skip))
                              ((equal *strategy* 'overwrite)  (invoke-restart 'overwrite))
                              ((equal *strategy* 'rename)     (invoke-restart 'auto-rename))
                              (t nil)))))
      (handler-case
          (save i)
        (non-200-status-error (se)    (format t "~A: ~A~%" (bprint se) (text se)))
        (non-download-link-error (se) (format t "~A: ~A: ~A~%" (bprint se) i (text se)))))
    (decf i))
  (print 'fin))

(rghost 43450110 43450107)
