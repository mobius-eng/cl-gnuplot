(in-package cl-gnuplot-2)

;; * Gnuplot plotting tool
;; ** Utilities
(defun string-join (delimiter strings)
  (format nil (format nil "~~{~~A~~^~A~~}" delimiter) strings))

;; ** Generics
(defgeneric render (object backend)
  (:documentation
   "Renders an object using a backend"))

;; ** Backends
(defvar *gnuplot* nil)

(defclass gnuplot-backend ()
  ((porcess)
   (stream)
   (command :initarg :commend :initform "gnuplot")))

(defmethod initialize-instance :after ((obj gnuplot-backend) &key)
  (with-slots (command process stream) obj
    (setf porcess (sb-ext:run-program
                   command
                   nil                    ; no arguments
                   :wait   nil      ; wait for child
                   :search t
                   :input  :stream  ; need an input stream
                   :error  :output  ; print to stdout for debugging
                   :output t))      ; print to stdout for debugging
    (setf stream (sb-ext:process-input process))
    (setf *gnuplot* obj))
  (let ((object-pointer (make-weak-pointer obj)))
    (finalize obj (lambda ()
                    (let ((object (weak-pointer-value object-pointer)))
                      (unless object
                        (with-slots (process) object
                          (sb-ext:process-close process))))))))


(defun send-gnuplot-command (gnuplot command)
  (with-slots (stream) gnuplot
    (format stream "~A~%" command)))


;; ** 2D data series
(defclass series2d ()
  ((data :initarg :data :accessor series2d-data))
  (:documentation "I represent data for 2D-plot. Data: ((x1 y1) (x2 y2) ...)"))

;; Ensure all the data in SINGLE-FLOAT format: gnuplot (by default) does not understand
;; suffixes =D= or =S=, but only =E= in floating-point data
(defmethod initialize-instance :after ((obj series2d) &key)
  (with-slots (data) obj
    (setf data
          (loop for (x y) in data
             collect `(,(coerce x 'single-float) ,(coerce y 'single-float))))))

(defmethod print-object ((obj series2d) out)
  (with-slots (data) obj
    (print-unreadable-object (obj out :type t)
      (format out "~A" data))))

;; ** Label
(defclass label ()
  ((text :initarg :label :accessor label-value)))

(defclass plot-label (label)
  ((object :initarg :object :accessor plot-label-object)))

(defmethod render ((obj plot-label) (backend gnuplot-backend))
  (with-slots (text object) obj
    (send-gnuplot-command backend
                          (format nil "set ~A ~A"
                                  object
                                  text))))
