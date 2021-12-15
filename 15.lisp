(load "utils.lisp")
(load "15-input.lisp")

(defvar *raw-input* *input-15*)

(defun create-map (input)
  "create part 1 cave map"
  (loop for line in (my-split input :delimiterp 'delimiter-newline)
        for y = 0 then (1+ y)
        with map = (make-hash-table)
        do (loop for c across line
                 for x = 0 then (1+ x) do
                   (setf (gethash (complex x y) map) (digit-char-p c)))
        finally (return map)))

(defvar *map* (create-map *raw-input*))

(defvar *xmax* (1- (length (first (my-split *raw-input* :delimiterp 'delimiter-newline)))))
(defvar *ymax* (1- (length (my-split *raw-input* :delimiterp 'delimiter-newline))))

(defun create-map-2 (map xmax ymax)
  "create part 2 cave map"
  (loop for i from 0 to 4
        with map-2 = (make-hash-table)
        do (loop for j from 0 to 4 do
                   (loop for k being the hash-keys in map using (hash-value v)
                         for x = (+ (realpart k) (* (1+ xmax) j))
                         for y = (+ (imagpart k) (* (1+ ymax) i))
                         for
                         sum = (+ v i j)
                         for new-v = (+ (mod sum 10) (floor (/ sum 10))) do
                           (setf (gethash (complex x y) map-2) new-v)))
        finally (return map-2)))

(defvar *map-2* (create-map-2 *map* *xmax* *ymax*))
(defvar *xmax-2* (1- (* (1+ *xmax*) 5)))
(defvar *ymax-2* (1- (* (1+ *ymax*) 5)))


; useful function to visualize map creation for part 2
(defun print-map (map xmax ymax)
  (loop for i from 0 to ymax do
          (loop for j from 0 to xmax do
                  (format t "~a" (gethash (complex j i) map)))
          (format t "~%")))

(defun get-neighbours (map pos)
  (loop for x in (list #c(-1 0) #c(1 0) #c(0 -1) #c(0 1))
        for potential-neighbour = (+ pos x)
        for potential-risk = (gethash potential-neighbour map)
        if potential-risk
        collect (list potential-neighbour potential-risk)))

(defun heuristic (a b)
  (+
   (abs (- (realpart b) (realpart a)))
   (abs (- (imagpart b) (imagpart a)))))

(defun a* (map start goal)
  (let ((frontier (list (list start 0)))
        (came-from (make-hash-table))
        (costs (make-hash-table)))
    (setf (gethash start costs) 0)

    (loop while (> (length frontier) 0)
          for (current current-risk) = (pop frontier)
          for neighbours = (get-neighbours map current) do
            (if (= current goal) (return))
            (loop for (n n-risk) in neighbours
                  for new-cost = (gethash n costs)
                  for new-cost-value = (+ (gethash current costs 0) n-risk) do
                    (when (or (not new-cost) (< new-cost-value new-cost))
                          (setf (gethash n costs) new-cost-value)
                          (push (list n (+ new-cost-value (heuristic goal n))) frontier)
						  ; sort the list to have the most promising element on top.
						  ;This is probably slower than using something like a priority queue, but it works well enough.
                          (setf frontier (sort frontier #'(lambda (x y) (< (second x) (second y)))))
                          (setf (gethash n came-from) current))))
    (list (gethash goal costs) came-from)))

(defun calc-total-risk (map xmax ymax)
  (first (a* map #c(0 0) (complex xmax ymax))))

(format t "~%answer 1: ~a" (calc-total-risk *map* *xmax* *ymax*))
(format t "~%answer 2: ~a" (calc-total-risk *map-2* *xmax-2* *ymax-2*))

(format T "~%")
