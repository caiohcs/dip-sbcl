(in-package :spatial-filters)

(defun round-transformation (function)
  (lambda (&rest args)
    (round (apply function args))))

(defun mean (&rest numbers)
  "Returns the mean of numbers."
  (/ (apply #'+ numbers)
     (length numbers)))

(defun mode (&rest numbers)
  "Returns the mode of numbers. TODO: rewrite this function."
  (let ((uniques (remove-duplicates numbers)))
    (first (first (sort (loop for i in uniques
			      collect (list i (loop for j in numbers
						    count (= i j))))
			(lambda (x y) (> (second x)
					 (second y))))))))

(defun median (&rest numbers)
  "Returns the median of numbers."
  (let* ((sorted (funcall (lambda (x) (sort x #'<)) numbers))
	 (list-size (length sorted))
	 (middle (nthcdr (1- (ceiling (/ list-size 2))) sorted)))
    (if (= 0 (mod list-size 2))
	(/ (+ (first middle) (second middle)) 2)
	(first middle))))

(defun gaussian (std m n)
  "Returns a m by n gaussian mask. m and n must be odd."
  (let* ((orig-i (/ (1- m) 2))
	 (orig-j (/ (1- n) 2))
	 (var (* std std)))
    (loop for i from 0 below m
	  nconc (loop for j from 0 below n
		      for dist-i = (abs (- i orig-i))
		      for dist-j = (abs (- j orig-j))
		      collect (* (/ 1 (* 2 pi var))
				 (exp (- (/ (+ (* dist-i dist-i) (* dist-j dist-j))
					    (* 2 var)))))))))

(defun define-mask (mask-coefs)
  "Returns a transformation created using the list mask-coefs.
In other words, returns a function that accepts a list of numbers
and then returns the result of a scalar product between its input and
mask-coenfs."
  (lambda (&rest image)
    (apply #'+ (mapcar (lambda (x y)
			 (* x y))
		       mask-coefs
		       image))))
