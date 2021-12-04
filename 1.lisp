(load "1-input.lisp")

;(format t "~{~a~^, ~}" INPUT)

(let ((count 0) (prev-value 10000))
  (dolist (value INPUT)
    ;(format t "value: ~d, " value)
    ;(format t "prev value: ~d, " prev-value)
    (if (< prev-value value)
      (incf count))
    (setf prev-value value))
  (format t "answer 1: ~d" count))

(let ((count 0) (prev-sum 30000))
  (dotimes (start-index (- (length INPUT) 2))
    ;(format t "start: ~d, " start-index)
    ;(format t "start item: ~d, " (nth start-index INPUT))
    (let ((sum
        (+
          (nth start-index INPUT)
          (nth (+ start-index 1) INPUT)
          (nth (+ start-index 2) INPUT))))
      ;(format t "sum: ~d | " sum)
      (if (< prev-sum sum)
        (incf count))
      (setf prev-sum sum)))
  (format t " answer 2: ~d" count))


