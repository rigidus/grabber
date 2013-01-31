(require 'cl-fad)
(require 'split-sequence)
(require 'alexandria)
(require 'cl-ppcre)
(setf ppcre:*use-bmh-matchers* nil)
(require 'drakma)
(require 'hunchentoot)
(require 'cl-json)


(defparameter *key* "AIzaSyDXNN-OfxFCBbo6owLD7SsJ7UiU9nxGLE8")
(defparameter *user-agent* "Mozilla/5.0 (X11; U; Linux i686; ru; rv:1.9.2.13) Gecko/20101206 Ubuntu/10.04 (lucid) Firefox/3.6.13")

(define-condition non-200-status-error (error)
    ((text :initarg :text :reader text)))

(define-condition non-download-link-error (error)
  ((text :initarg :text :reader text)))


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
         (fname (car (last (puri:uri-parsed-path (puri:parse-uri link))))))
    (multiple-value-bind (body-or-stream status-code headers uri stream-out must-close reason-phrase)
        (drakma:http-request (get-page number #'get-link))
      (values
       fname
       body-or-stream))))

(defun save (number)
  (multiple-value-bind (fname fdata)
      (download number)
    (let ((stream-type (cadr (type-of fdata))))
      (with-open-file (stream (pathname (format nil "./files/~A" fname))
                              :element-type stream-type
                              :direction :output
                              :if-exists :supersede)
            (write-sequence fdata stream)))))

;; (multiple-value-bind (fname fdata)
;;     (download 43450113)
;;   (cadr (type-of fdata)))

(defun rghost(start end)
  (print 'rghost)
  (do ((i start))
      ((< i end))
    (print i)
    (handler-case
        (save i)
      (non-200-status-error (se)    (print (list se (text se))))
      (non-download-link-error (se) (print (list se (text se)))))
    (decf i))
  (print 'fin))

(rghost 43450111 43450100)
