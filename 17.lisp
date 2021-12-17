(load "utils.lisp")
(load "17-input.lisp")

(defun parse-input (inpt)
  (loop for x in (cddr (my-split inpt :delimiterp 'delimiter-space))
        for y = (subseq (remove #\, x) 2)
        for z = (my-split y :delimiterp #'(lambda (c) (char= c #\.)))
        collect (mapcar 'parse-integer z)))


;list of lists of the form: ((xmin xmax) (ymin ymax))
(defvar *target-area* (parse-input *input-17*))

(defun calc-step (x y velx vely)
  (incf x velx)
  (incf y vely)
  (cond ((> velx 0) (incf velx -1))
        ((< velx 0) (incf velx)))
  (incf vely -1)
  (list x y velx vely))

(defun part-1 (target-area)
  ;I'm making in this function the assumption that the target is always to the bottom right.
  (let* ((xtarget (first target-area))
         (ytarget (second target-area))
         (xmin (first xtarget))
         (xmax (second xtarget))
         (ymin (first ytarget))
         (ymax (second ytarget)))
    ;Iteration borders could probably be optimized a lot, but performance is not a concern here.
    (loop for start-velx from (- xmax) to xmax
          maximize (loop for start-vely from ymin to (- ymin)
                         maximize
                         (loop for step from 0 to 1000
                               for (x y velx vely) = (list 0 0 start-velx start-vely) then (calc-step x y velx vely)
                               while (and (<= x xmax) (>= y ymin))
                               maximize y into highest
                               if (and (<= x xmax) (>= x xmin) (<= y ymax) (>= y ymin))
                               do (return highest)
                               finally (return 0))))))


;ugly copy paste will save me some well deserved time tonight :D
(defun part-2 (target-area)
  ;I'm making in this function the assumption that the target is always to the bottom right.
  (let* ((xtarget (first target-area))
         (ytarget (second target-area))
         (xmin (first xtarget))
         (xmax (second xtarget))
         (ymin (first ytarget))
         (ymax (second ytarget)))
    ;Iteration borders could probably be optimized a lot, but performance is not a concern here.
    (loop for start-velx from (- xmax) to xmax
          sum (loop for start-vely from ymin to (- ymin)
                    sum
                    (loop for step from 0 to 1000
                          for (x y velx vely) = (list 0 0 start-velx start-vely) then (calc-step x y velx vely)
                          while (and (<= x xmax) (>= y ymin))
                          if (and (<= x xmax) (>= x xmin) (<= y ymax) (>= y ymin))
                          do (return 1)
                          finally (return 0))))))

(format t "~%answer 1: ~a" (part-1 *target-area*))
(format t "~%answer 2: ~a" (part-2 *target-area*))

(format t "~%")
