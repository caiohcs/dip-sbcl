(load "~/quicklisp/setup.lisp")
(push (uiop:parse-unix-namestring "./fftw_bindings/") asdf:*central-registry*)
(ql:quickload "fftw-bindings")
(ql:quickload "threading-macros")

(defpackage :frequency-filters
  (:use :cl)
  (:export #:filter-low-pass-ideal
	   #:filter-high-pass-ideal
	   #:filter-low-pass-butterworth
	   #:filter-high-pass-butterworth
	   #:filter-low-pass-gaussian
	   #:filter-high-pass-gaussian))

(defpackage :spatial-filters
  (:use :cl)
  (:export #:mean
	   #:mode
	   #:median
	   #:gaussian
	   #:define-mask))

(defpackage :textures
  (:use :cl)
  (:export #:co-occurrence-energy
	   #:co-occurrence-entropy
	   #:co-occurrence-variance))

(defpackage :transformations
  (:use :cl)
  (:export #:negative
	   #:logarithm
	   #:gamma
	   #:contrast-stretching))

(defpackage :image-processing
  (:use #:cl
	#:threading-macros
	#:transformations
	#:spatial-filters
	#:frequency-filters
	#:textures
	#:fftw-bindings))
