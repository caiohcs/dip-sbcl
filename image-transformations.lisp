(in-package :transformations)

(defun negative (max-level)
  "Return a transformation T(X) = max-level - X."
  (lambda (x) (- max-level x)))

(defun logarithm (max-level)
  "Return a transformation T(X) = c*log(1 + X)."
  (lambda (x) (ceiling (* max-level
			  (/ (log (1+ x) 10)
			     (log (1+ max-level) 10))))))

(defun gamma (max-level gamma &optional (offset 0))
  "Return a transformation T(X) = c*(offset + X)^gamma."
  (lambda (x) (ceiling (* max-level
			  (/ (expt (+ offset x) gamma)
			     (expt (+ offset max-level) gamma))))))

(defun contrast-stretching (max-level a b c d)
  "Return a transformation T(X) using (a b) (c d). "
  (lambda (x)
    (setf x (/ x max-level))
    (ceiling (* max-level (cond ((<= x a)
				 (* b x))
				((and (> x a) (< x c))
				 (let* ((delta (/ (- d b) (- c a)))
					(k (- b (* delta a))))

				   (+ k (* delta x))))
				(t
				 (let* ((delta (/ (- 1 d) (- 1 c)))
					(k (- 1 delta)))
				   (+ k (* delta x)))))))))
