(load "utils.lisp")
(load "13-input.lisp")

(defvar *points* (make-hash-table))
(defvar *folds* nil)

(defun get-folds-from-input (input-13)
  (loop for line in (my-split input-13 :delimiterp 'delimiter-newline)
        if (and (> (length line) 0) (char= (char line 0) #\f))
        do (push
            (my-split
             (third (my-split line :delimiterp 'delimiter-space))
             :delimiterp #'(lambda (x) (char= #\= x)))
            *folds*))
  (setf *folds* (reverse *folds*)))

(defun get-points-from-input (input-13)
  (loop for line in (my-split input-13 :delimiterp 'delimiter-newline)
        if (and (> (length line) 0) (char/= (char line 0) #\f))
        do (destructuring-bind (x y)
               (mapcar 'parse-integer (my-split line :delimiterp 'delimiter-comma))
             (setf (gethash (complex x y) *points*) 1))))

(defun parse-input (input-13)
  (get-folds-from-input input-13)
  (get-points-from-input input-13))

(parse-input *input-13*)

(defun apply-fold (fold points)
  (let ((new-points (make-hash-table))
        (direction (first fold))
        (value (parse-integer (second fold))))
    (cond ((string= "y" direction)
           (loop for pos being the hash-keys in points
                 for x = (realpart pos)
                 for y = (imagpart pos) do
                   (if (> y value)
                       (setf (gethash (complex x (- (* value 2) y)) new-points) 1)
                       (setf (gethash (complex x y) new-points) 1))))
          ((string= "x" direction)
           (loop for pos being the hash-keys in points
                 for x = (realpart pos)
                 for y = (imagpart pos) do
                   (if (> x value)
                       (setf (gethash (complex (- (* value 2) x) y) new-points) 1)
                       (setf (gethash (complex x y) new-points) 1)))))
    new-points))

(defun hash-length (ht)
  (loop for k being the hash-keys in ht sum 1))

(defun part-1 (points folds)
  (hash-length (apply-fold (first folds) points)))

(defun print-points (points)
  (destructuring-bind (xmax ymax)
      (loop for pos being the hash-keys in points
            maximize (realpart pos) into xmax
            maximize (imagpart pos) into ymax
            finally (return (list xmax ymax)))
    (format t "~%")
    (dotimes (j (1+ ymax))
             (dotimes (i (1+ xmax))
                      (if (gethash (complex i j) points) (format t "#") (format t ".")))
             (format t "~%"))))

(defun part-2 (points folds)
  (print-points
   (loop for fold in folds
         for new-points = (apply-fold fold points) then (apply-fold fold new-points)
         finally (return new-points))))

(format t "~% answer 1: ~a~%" (part-1 *points* *folds*))
(part-2 *points* *folds*)
(format t "~% answer 2: see above")

(format T "~%")
