(load "utils.lisp")
(load "6-input.lisp")

;;; parsing

(defvar *fish-timers* (mapcar 'parse-integer (my-split *input-test* :delimiterp 'delimiter-comma)))

;;;; naive solution for part a

(defun naive-step (timers)
  (let ((new-timers nil)
        (new-fishes 0))
    (dolist (timer timers)
      (push (if (= timer 0)
                (progn (incf new-fishes) 6)
                (decf timer))
            new-timers))
    (dotimes (i new-fishes)
      (push 8 new-timers))
    (reverse new-timers)))

(defun naive-sol (timers days)
  (loop for i from 0 to days
        for new-timers = timers then (naive-step new-timers)
        finally (return new-timers)))

(format t "~% answer 1: ~a" (length (naive-sol *fish-timers* 80)))


;;;; wrong solutions, that avoid the stack overflow (probably) but take too much time (minutes on the test input)

;calculate numbers of descendants of fish + 1 (1 being the fish itself)
(defun calc-total-fishes-from-one (timer days)
  (if (>= timer days)
      (return-from calc-total-fishes-from-one 1))
  (+ (calc-total-fishes-from-one-alt 6 (- days timer 1)) (calc-total-fishes-from-one-alt 8 (- days timer 1))))

(defun calc-total-fishes-from-one-smart (timer days end)
  (do ((tm timer 7) (ds (+ days timer) (+ ds 7)) (total 1))
    ((> ds end) total)
    (if (> (+ ds 9) end)
        (incf total 1)
        (incf total (calc-total-fishes-from-one-smart 8 (+ ds 1) end)))))

(defun better-sol (timers days)
  (loop for timer in timers summing (calc-total-fishes-from-one-smart timer 0 (1- days))))


; (println (better-sol *fish-timers* 18)) ;takes way too much time


;  best solution: fast and no stack overflow

(defun best-sol (timers days)
  (let ((fishes (loop for i from 0 to 8 collecting (count-if #'(lambda (x) (= x i)) timers))))

    (loop for d from 0 to (1- days)
          for tmp0 = (nth 0 fishes) do
            (progn
             (loop for i from 0 to 8 do
                     (if (= i 8)
                         (setf (nth i fishes) tmp0)
                         (setf (nth i fishes) (nth (1+ i) fishes))))
             (setf (nth 6 fishes) (+ (nth 6 fishes) tmp0))))
    (apply '+ fishes)))

(format t "~% answer 2: ~a" (best-sol *fish-timers* 256))

(format t "~%")
