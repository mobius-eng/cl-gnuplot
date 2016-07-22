(in-package cl-user)

#-sbcl
(error "System CL-GNUPLOT uses SBCL specific commands")

(defpackage #:cl-gnuplot
  (:use #:cl)
  (:export
   #:make-series-2d
   #:make-plot
   #:plot-title
   #:plot-xlabel
   #:plot-ylabel
   #:plot-x2label
   #:plot-y2label
   #:plot-cmd
   #:plot-draw
   #:plot-close
   #:plot-add-series
   #:plot-remove-series))


(defpackage :cl-gnuplot2
  (:use #:cl #:trivial-garbage))
