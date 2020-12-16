(load "~/quicklisp/setup.lisp")
(push (uiop:parse-unix-namestring "./fftw_bindings/") asdf:*central-registry*)
(ql:quickload "fftw-bindings")
(ql:quickload "threading-macros")

(defpackage :image-processing
  (:use #:cl
	#:threading-macros
	#:fftw-bindings))
