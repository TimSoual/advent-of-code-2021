(load "utils.lisp")
(load "7-input.lisp")

(defvar *ship-positions* (mapcar 'parse-integer (my-split *input-7* :delimiterp 'delimiter-comma)))

(defun part-1 (ships)
  "ships is a list of initial positions of crab ships"
  (let ((ship-number-by-point (make-hash-table))
        (total-fuel-by-pos (make-hash-table))
        (cur-total-fuel 0))
    (dolist (ship ships)
            (if (gethash ship ship-number-by-point)
                (incf (gethash ship ship-number-by-point))
                (setf (gethash ship ship-number-by-point) 1)))

    (dotimes (pos (apply 'max ships))
             (setf cur-total-fuel 0)
             (maphash #'(lambda (ship ship-num) (incf cur-total-fuel (* ship-num (abs (- ship pos))))) ship-number-by-point)
             (setf (gethash pos total-fuel-by-pos) cur-total-fuel))
    (loop for v being the hash-values in total-fuel-by-pos minimize v)))


(defun sum-first-integers (end)
  "returns the sum of integers from 1 to n"
  (/ (* end (1+ end)) 2))

; Only the use of sum-first-integers in the maphash lambda is different from part 1.
; I could probably refactor this, but it's not really worth it for me right now.
(defun part-2 (ships)
  "ships is a list of initial positions of crab ships"
  (let ((ship-number-by-point (make-hash-table))
        (total-fuel-by-pos (make-hash-table))
        (cur-total-fuel 0))
    (dolist (ship ships)
            (if (gethash ship ship-number-by-point)
                (incf (gethash ship ship-number-by-point))
                (setf (gethash ship ship-number-by-point) 1)))
    (dotimes (pos (apply 'max ships))
             (setf cur-total-fuel 0)
             (maphash #'(lambda (ship ship-num) (incf cur-total-fuel (* ship-num (sum-first-integers (abs (- ship pos)))))) ship-number-by-point)
             (setf (gethash pos total-fuel-by-pos) cur-total-fuel))
    (loop for v being the hash-values in total-fuel-by-pos minimize v)))

(format T "~% answer 1: ~a" (part-1 *ship-positions*))
(format T "~% answer 2: ~a" (part-2 *ship-positions*))

(format T "~%")
