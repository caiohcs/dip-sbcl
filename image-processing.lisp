;;; Load packages
(load "packages.lisp")
(load "image-frequency-filters.lisp")
(load "image-histogram.lisp")
(load "image-spatial-filters.lisp")
(load "image-textures.lisp")
(load "image-transformations.lisp")

(in-package :image-processing)

;;; Classes
(defclass pixel ()
  ((value
    :initarg :value
    :accessor pixel-value)))

(defclass image ()
  ((pixels :initarg :pixels)
   (width :initarg :width)
   (height :initarg :height)
   (max-intensity-level :initarg :max-intensity-level)
   (type :initarg :type)))

;;; Getters
(defun matrix-value (matrix i j)
  (svref (svref matrix i) j))

(defun image-pixel (image i j)
  (with-slots (pixels) image
    (matrix-value pixels i j)))

(defun image-pixel-value (image i j)
  (pixel-value (image-pixel image i j)))

;;; Setters
(defun (setf matrix-value) (value matrix i j)
  (setf (svref (svref matrix i) j) value))

(defun (setf image-pixel) (pixel image i j)
  (with-slots (pixels) image
    (setf (matrix-value pixels i j) pixel)))

(defun (setf image-pixel-value) (pixel-value image i j)
  (setf (image-pixel image i j) (make-instance 'pixel :value pixel-value)))

;;; Copy
(defun matrix-copy (matrix)
  (map 'vector #'copy-seq matrix))

(defun image-copy (image)
  (with-slots (pixels width height max-intensity-level type) image
    (make-instance 'image :type type
			  :width width
			  :height height
			  :max-intensity-level max-intensity-level
			  :pixels (matrix-copy pixels))))

;;; Map
(defun map-matrix (function matrix &key
				     map-a-copyp
				     (reader (lambda (matrix i j)
					       (list (matrix-value matrix i j)))))
  (let* ((matrix-to-extract (if map-a-copyp
				(matrix-copy matrix)
				matrix))
	 (height (array-dimension matrix 0))
	 (width (array-dimension (svref matrix 0) 0)))
    (loop for i from 0 below height
	  do (loop for j from 0 below width
		   do (setf (matrix-value matrix i j)
			    (apply function
				   (funcall reader matrix-to-extract i j))))))
  matrix)

(defun map-image (function image &key reader map-a-copyp)
  (with-slots (pixels) image
    (map-matrix #'pixel-value pixels)
    (if reader
	(map-matrix function pixels :reader reader :map-a-copyp map-a-copyp)
	(map-matrix function pixels :map-a-copyp map-a-copyp))
    (map-matrix (lambda (value) (make-instance 'pixel :value value)) pixels))
  image)

;;; Reduce
(defun reduce-matrix (function matrix)
  (->> matrix
       (map 'vector (lambda (line)
		      (reduce function line)))
       (reduce function)))

(defun img-transform (filename output-extension transf &key mask-dimensions)
  "Transform the image filename using the transformation function transf T(x)."
  (let* ((file-words (read-file-words
		      (concatenate 'string filename ".pgm")))
	 (image (img-raw-pgm-to-image file-words)))
    (with-slots (height width) image
      (cond (mask-dimensions
	     (map-image transf image :map-a-copyp t
				     :reader (lambda (pixels i j)
					       (matrix-values-around-point pixels height width i j
									   (first mask-dimensions)
									   (second mask-dimensions)))))
	    (t
	     (map-image transf image))))
    (img-save image (concatenate 'string filename output-extension))))

;;; Frequency domain
(defun matrix-shift (matrix)
  (->> matrix
       (vector-shift)
       (transpose)
       (vector-shift)
       (transpose)))

(defun matrix-ft (function matrix)
  (->> matrix
       (map 'vector function)
       (transpose)
       (map 'vector function)
       (transpose)))

(defun img-frequency-transform (filename output-extension transf)
  "Transform the image filename using the transformation function transf T(x)."
  (let* ((file-words (read-file-words
		      (concatenate 'string filename ".pgm")))
	 (image (img-raw-pgm-to-image file-words))
	 (pixels (map-matrix #'pixel-value (slot-value image 'pixels)))
	 (reader (lambda (matrix i j)
		   (list (matrix-value matrix i j) i j)))
	 (transf (with-slots (width height) image
		   (funcall transf (round (/ height 2)) (round (/ width 2))))))

    (setf (slot-value image 'pixels)
	  (as-> pixels PLACEHOLDER
		(matrix-ft #'dft PLACEHOLDER)
		(matrix-shift PLACEHOLDER)
		(map-matrix transf PLACEHOLDER :reader reader)
		(matrix-shift PLACEHOLDER)
		(matrix-ft #'idft PLACEHOLDER)
		(map-matrix #'realpart PLACEHOLDER)
		(normalize-min-max PLACEHOLDER (slot-value image 'max-intensity-level))
		(map-matrix (lambda (value) (make-instance 'pixel :value (round value))) PLACEHOLDER)))
    (img-save image (concatenate 'string filename output-extension))))

(defun img-co-occurrence (image coord-transf)
  "Returns the co-occurence matrix of image using the coordinate transformation function
coord-transf. The co-occurence matrix is a square matrix of dimensions number-of-levels ^ 2.
co-occurence(i, j) = how many times the intensity i is positioned relatively (according to
coord-transf) to j."
  (with-slots (width height max-intensity-level) image
    (let* ((num-levels (1+ max-intensity-level))
	   (res (make-array num-levels)))
      (loop for i from 0 below num-levels
	    do (setf (svref res i) (make-array num-levels :initial-element 0)))

      (loop for i from 0 below height
	    do (loop for j from 0 below width
		     do (let* ((level-j (image-pixel-value image i j))
			       (other-coord (funcall coord-transf i j))
			       (other-coord-x (first other-coord))
			       (other-coord-y (second other-coord))
			       (level-i nil))
			  (when (and (>= other-coord-x 0) (< other-coord-x height)
				     (>= other-coord-y 0) (< other-coord-y width))
			    (setf level-i (image-pixel-value image other-coord-x other-coord-y))
			    (incf (matrix-value res level-i level-j))))))
      res)))

(defun read-file-words (filename)
  "Read a file and return a list formed by its words."
  (remove "" (uiop:split-string
	      (with-open-file (in filename) (uiop:read-file-string in))
	      :separator (format nil "~C~C~C~C"
				 #\return
				 #\linefeed
				 #\space
				 #\newline))
	  :test #'equal))

(defun img-save (image filename)
  (with-slots (pixels width height max-intensity-level type) image
    (with-open-file (out filename
			 :direction :output
			 :if-exists :supersede)
      (format out "~a~%~a ~a~%~a"
	      type width height max-intensity-level)
      (loop for i from 0 below (slot-value image 'height)
	    do (format out "~%")
	       (loop for j from 0 below (slot-value image 'width)
		     do (format out "~a " (slot-value (svref (svref pixels i) j) 'value)))))))

(defun img-raw-pgm-to-image (img-raw-pgm)
  "Returns an instance of the class image created using img-raw-pgm."
  (let* ((img-type (first img-raw-pgm))
	 (width (parse-integer (second img-raw-pgm)))
	 (height (parse-integer (third img-raw-pgm)))
	 (max-level (parse-integer (fourth img-raw-pgm)))
	 (raw-values (nthcdr 4 img-raw-pgm))
	 (pixels (make-array height)))
    (loop for i from 0 below height do (setf (svref pixels i) (make-array width)))

    (loop for i from 0 below height
	  do (loop for j from 0 below width
		   do (setf (svref (svref pixels i) j)
			    (make-instance 'pixel :value (parse-integer (pop raw-values))))))

    (make-instance 'image :type img-type
			  :width width
			  :height height
			  :max-intensity-level max-level
			  :pixels pixels)))

(defun img-texture (filename coord-transf)
  "Transform the image filename using the transformation function transf T(x)."
  (let* ((file-words (read-file-words
		      (concatenate 'string filename ".pgm")))
	 (image (img-raw-pgm-to-image file-words))
	 (co-occurrence (img-co-occurrence image coord-transf))
	 (sum-texture (reduce-matrix #'+ co-occurrence)))
    (map-matrix (lambda (x) (/ x sum-texture)) co-occurrence)

    (let* ((reader (lambda (matrix i j)
		     (list (matrix-value matrix i j) i j)))
	   (entropy (as-> (matrix-copy co-occurrence) PLACEHOLDER
			  (map-matrix #'co-occurrence-entropy PLACEHOLDER :reader reader)
			  (reduce-matrix #'+ PLACEHOLDER)))
	   (energy (as-> (matrix-copy co-occurrence) PLACEHOLDER
			 (map-matrix #'co-occurrence-energy PLACEHOLDER :reader reader)
			 (reduce-matrix #'+ PLACEHOLDER)))
	   (variance (as-> (matrix-copy co-occurrence) PLACEHOLDER
			   (map-matrix #'co-occurrence-variance PLACEHOLDER :reader reader)
			   (reduce-matrix #'+ PLACEHOLDER))))
      (list :entropy entropy
	    :energy (* 1.0 energy)
	    :variance (* 1.0 variance)))))

(defun coordinates-around-point (x y p q m n)
  "Returns a list with the coordinates of all points corresponding to a
matrix p by q around (x,y). The points are limited by (0,0) and (m,n)."
  (loop for i from (- x (/ (1- p) 2)) to (+ x (/ (1- p) 2))
	nconc (loop for j from (- y (/ (1- q) 2)) to (+ y (/ (1- q) 2))
		    collect (list
			     (if (< i 0)
				 0 (min i m))
			     (if (< j 0)
				 0 (min j n))))))

(defun matrix-values-around-point (pixels height width x y p q)
  "Returns a list with the values of all points of image corresponding to
a matrix p by q around (x,y)."
  (loop for (i j) in (coordinates-around-point x y p q (1- height) (1- width))
	collect (matrix-value pixels i j)))

(defun transpose (arr)
  "Transpose a matrix."
  (let* ((M (array-dimension arr 0))
	 (N (array-dimension (svref arr 0) 0))
	 (output (make-array N :element-type 'vector)))
    (loop for i from 0 below N do (setf (svref output i) (make-array M)))
    (loop for i below N
	  do (loop for line across arr
		   for j below M
		   do (setf (svref (svref output i) j) (svref line i))))
    output))

(defun normalize-min-max (matrix &optional (new-max 1))
  "Normalizes a matrix using min-max."
  (let ((min-val (reduce-matrix #'min matrix))
	(max-val (reduce-matrix #'max matrix)))
    (map-matrix (lambda (pixel)
		  (* new-max (/ (- pixel min-val)
				(- max-val min-val))))
		matrix)))

(defun vector-shift (vector)
  "Shifts the lower and the upper half of arr."
  (let* ((N (array-dimension vector 0))
	 (half (ceiling (/ N 2))))
    (concatenate 'vector (subseq vector half N)
		 (subseq vector 0 half))))

(defun image-from-file (filename)
  "Returns an image object created from filename."
  (let ((file-words (read-file-words (concatenate 'string filename ".pgm"))))
    (img-raw-pgm-to-image file-words)))

;;;; The code below sucks, I'll rewrite it later..
;;; TODO: remove duplicated code between raw-pgm and raw-ppm
(defun img-raw-ppm-to-image (img-raw-ppm)
  "Returns an instance of the class image created using img-raw-ppm."
  (let* ((img-type (first img-raw-ppm))
	 (width (parse-integer (second img-raw-ppm)))
	 (height (parse-integer (third img-raw-ppm)))
	 (max-level (parse-integer (fourth img-raw-ppm)))
	 (raw-values (nthcdr 4 img-raw-ppm))
	 (pixels (make-array height)))
    (loop for i from 0 below height do (setf (svref pixels i) (make-array width)))

    (loop for i from 0 below height
	  do (loop for j from 0 below width
		   do (setf (svref (svref pixels i) j)
			    (make-instance 'pixel
					   :value (loop for i from 0 below 3
							collect (parse-integer
								 (pop raw-values)))))))

    (make-instance 'image :type img-type
			  :width width
			  :height height
			  :max-intensity-level max-level
			  :pixels pixels)))

;;; TODO: remove duplicated code between image-transform and image-transform ppm
(defun img-transform-ppm (filename output-extension transf &key mask-dimensions)
  "Transform the image filename using the transformation function transf T(x)."
  (let* ((file-words (read-file-words
		      (concatenate 'string filename ".ppm")))
	 (image (img-raw-ppm-to-image file-words)))
    (with-slots (height width) image
      (cond (mask-dimensions
	     (map-image transf image :map-a-copyp t
				     :reader (lambda (pixels i j)
					       (matrix-values-around-point pixels height width i j
									   (first mask-dimensions)
									   (second mask-dimensions)))))
	    (t
	     (map-image-ppm transf image))))
    (img-save-ppm image (concatenate 'string filename output-extension))))

;;; TODO: remove duplicated code between map-image-ppm and map-image
(defun map-image-ppm (function image &key reader map-a-copyp)
  (with-slots (pixels) image
    (map-matrix #'pixel-value pixels)
    (let* ((pixels-red (map 'vector (lambda (line) (map 'vector #'first line)) pixels))
	   (pixels-green (map 'vector (lambda (line) (map 'vector #'second line)) pixels))
	   (pixels-blue (map 'vector (lambda (line) (map 'vector #'third line)) pixels))
	   (pixels-rgb (list pixels-red pixels-green pixels-blue)))
      (loop for pixels-list in pixels-rgb
	    do (if reader
		   (map-matrix function pixels-list :reader reader :map-a-copyp map-a-copyp)
		   (map-matrix function pixels-list :map-a-copyp map-a-copyp)))
      (setq pixels (map 'vector (lambda (r-line g-line b-line)
				  (map 'vector
				       (lambda (r g b)
					 (make-instance 'pixel :value (list r g b)))
				       r-line g-line b-line))
			pixels-red pixels-green pixels-blue)))
    image))

;;; TODO: remove duplicated code between img-save and img-save-ppm
(defun img-save-ppm (image filename)
  (with-slots (pixels width height max-intensity-level type) image
    (map-matrix #'pixel-value pixels)
    (with-open-file (out filename
			 :direction :output
			 :if-exists :supersede)
      (format out "~a~%~a ~a~%~a"
	      type width height max-intensity-level)
      (loop for i from 0 below (slot-value image 'height)
	    do (format out "~%")
	       (loop for j from 0 below (slot-value image 'width)
		     do (format out "~a ~a ~a " (first (svref (svref pixels i) j))
				(second (svref (svref pixels i) j))
				(third (svref (svref pixels i) j))))))))
