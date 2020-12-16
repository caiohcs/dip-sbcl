(in-package :image-processing)

;;; Low pass filters 
(defmacro deflowpass (low-pass-name args docstring &rest body)
  "Macro used to create low pass filters. The body can access the following variables:
center-i, center-j, pix, i, j and distance."
  `(defun ,low-pass-name ,args
     ,docstring
     (lambda (center-i center-j)
       (lambda (pix i j)
	 (let ((distance (sqrt (+ (expt (- i center-i) 2)
				  (expt (- j center-j) 2)))))
	   ,@body)))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (deflowpass filter-low-pass-ideal (R)
    "Returns a low pass filter function with center (center-i, center-j) and radius R."
    (if (<= distance R) pix 0))

  (deflowpass filter-low-pass-butterworth (R n)
    "Returns a low pass butterworth filter function with center (center-i, center-j) and radius R."
    (/ pix (+ 1 (expt (/ distance R) (* 2 n)))))

  (deflowpass filter-low-pass-gaussian (R)
    "Returns a low pass gaussian filter function with center (center-i, center-j) and radius R."
    (* pix (exp (* -1 (/ (expt distance 2) (* 2 (expt R 2))))))))

;;; High pass filters
(defmacro defhighpass (high-pass-name (filter-low-pass &rest args) docstring)
  "Macro used to create a high pass filter using 1 - low pass filter."
  `(defun ,high-pass-name (,@args)
     ,docstring
     (let ((low-pass-R (,filter-low-pass ,@args)))
       (lambda (center-i center-j)
	 (let ((low-pass (funcall low-pass-R center-i center-j)))
	   (lambda (pix i j)
	     (- pix (funcall low-pass pix i j))))))))

(defhighpass filter-high-pass-ideal (filter-low-pass-ideal R)
	     "Returns a high pass ideal filter function with center (center-i, center-j) and radius R.")

(defhighpass filter-high-pass-butterworth (filter-low-pass-butterworth R n)
	     "Returns a high pass n-degree butterworth filter function with center
(center-i, center-j) and radius R.")

(defhighpass filter-high-pass-gaussian (filter-low-pass-gaussian R)
	     "Returns a high pass gaussian filter function with center (center-i, center-j) and radius R.")
