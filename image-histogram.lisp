(in-package :image-processing)

(defun write-histogram-to-file (filename extension hist)
  "Write the histogram values as a single column to a file."
  (with-open-file (out (concatenate 'string filename extension)
		       :direction :output
		       :if-exists :supersede)
    (format out "~{~a~%~}" hist)))

(defun calc-hist (max-level pixels-mat)
  "Calculate the histogram of an image."
  (let ((n-lines (array-dimension pixels-mat 0)))
    (loop for level from 0 to max-level
	  collect (loop for i from 0 below n-lines
			sum (count level (svref pixels-mat i))))))

(defun norm-hist (hist num-pixels)
  "Normalize a histogram."
  (mapcar (lambda (x)
	    (/ x num-pixels))
	  hist))

(defun calc-cdf (hist-norm)
  "Calculate the CDF using a normalized histogram."
  (let ((sum 0))
    (loop for prob in hist-norm
	  collect (setq sum (+ sum prob)))))


(defun calc-hist-equal-alist (max-level cdf)
  "Return an alist for histogram equalization with (OLD-VALUE NEW-VALUE)
elements."
  (let ((i -1))
    (mapcar (lambda (x)
	      (setf i (1+ i))
	      `(,i ,(ceiling (* x max-level))))
	    cdf)))

(defun histogram-equalization (filename)
  "Return a transformation T(X) = c*(offset + X)^gamma."
  (let ((image (image-from-file filename)))
    (with-slots (width height max-intensity-level pixels) image
      (let* ((pixels-values (map-matrix #'pixel-value pixels))
	     (hist (calc-hist max-intensity-level pixels-values))
	     (hist-norm (norm-hist hist (* width height)))
	     (cdf (calc-cdf hist-norm))
	     (hist-old-new-alist (calc-hist-equal-alist max-intensity-level cdf)))
	(lambda (x)
	  (second (assoc x hist-old-new-alist)))))))
