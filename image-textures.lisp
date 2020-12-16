(in-package :image-processing)

(defun co-occurrence-energy (p-i-j &optional i j)
  "Returns p-i-j^2"
  (* p-i-j p-i-j))

(defun co-occurrence-entropy (p-i-j &optional i j)
  "Returns p-i-j * log(p-i-j)."
  (if (> p-i-j 0)
      (* p-i-j (log p-i-j))
      0))

(defun co-occurrence-variance (p-i-j &optional i j)
  "Returns p-i-j * (i-j)^2."
  (* (expt (- i j) 2) p-i-j))
