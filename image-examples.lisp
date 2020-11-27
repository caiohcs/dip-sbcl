;;;; Examples

;;; TODO - broken - to fix
(img-transform "img/pepper" "-high-pass.pgm" (define-mask '(-1 -1 -1
							    -1 8 -1
							    -1 -1 -1))
	       :mask-dimensions '(3 3))
(img-transform "img/monalisa" "-gaussian.pgm" (define-mask (gaussian 1.3 5 5))
	       :mask-dimensions '(5 5))

;;; Histogram equalization
(img-transform "img/mona" "-equal.pgm" (histogram-equalization "img/mona"))

;;; Intensity transformations
(img-transform "img/pepper" "-neg.pgm" (negative 255))
(img-transform "img/relogio" "-gamma.pgm" (gamma 255 0.3))
(img-transform "img/mountain" "-contrast.pgm" (contrast-stretching 255 0.6 0 0.6 1))
(img-transform "img/mona" "-log.pgm" (logarithm 255))

;;; Masks
(img-transform "img/pepper" "-mean.pgm" #'median :mask-dimensions '(7 7))
(img-transform "img/pepper" "-max.pgm" #'max :mask-dimensions '(3 3))

;;; Texture
(img-texture "img/brain" (lambda (x y) (list (1+ x) (1+ y))))

;;; Frequency transformations
(img-frequency-transform "img/pepper" "-fft.pgm" (filter-high-pass-ideal 10))
(img-frequency-transform "img/pepper" "-fft.pgm" (filter-high-pass-butterworth 10 1))
(img-frequency-transform "img/pepper" "-fft.pgm" (filter-high-pass-gaussian 10))
(img-frequency-transform "img/pepper" "-fft.pgm" (filter-low-pass-butterworth 10 1))
(img-frequency-transform "img/pepper" "-fft.pgm" (filter-low-pass-ideal 10))
(img-frequency-transform "img/pepper" "-fft.pgm" (filter-low-pass-gaussian 10))
