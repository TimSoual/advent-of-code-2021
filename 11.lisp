(load "utils.lisp")
(load "11-input.lisp")

(defvar *steps* 100)
(defvar *raw-input* *input-11*)
(defvar *input-size-x* (length (first (my-split *raw-input* :delimiterp 'delimiter-newline))))
(defvar *input-size-y* (length (my-split *raw-input* :delimiterp 'delimiter-newline)))

;I'm trying a new approach with a hashmap and not 2d lists
(defun create-dumbos-map ()
  (loop for line in (my-split *raw-input* :delimiterp 'delimiter-newline)
        for y from 0 to *input-size-y*
        with hashtable = (make-hash-table)
        do (loop for x from 0 to *input-size-x*
                 for charac across line do
                   (setf (gethash (complex x y) hashtable) (digit-char-p charac)))
        finally (return hashtable)))

(defun is-outside-map (pos)
  (let ((x (realpart pos))
        (y (imagpart pos)))
    (or (or (< x 0) (> x (1- *input-size-x*))) (or (< y 0) (> y (1- *input-size-y*))))))

(defun get-flashable-neighbours (pos dumbos)
  "for a position get neighbours that have not already flashed in this step"
  (let ((x (realpart pos))
        (y (imagpart pos))
        (neighbours nil))

    ; could loop but this is nicer imo
    (push (complex (1- x) (1- y)) neighbours)
    (push (complex x (1- y)) neighbours)
    (push (complex (1+ x) (1- y)) neighbours)

    (push (complex (1- x) y) neighbours)
    (push (complex (1+ x) y) neighbours)

    (push (complex (1- x) (1+ y)) neighbours)
    (push (complex x (1+ y)) neighbours)
    (push (complex (1+ x) (1+ y)) neighbours)

    ; remove positions outside the map
    (setf neighbours (remove-if 'is-outside-map neighbours))
    ; remove neighbours that have already flashed this step
    (remove-if #'(lambda (n) (> (gethash n dumbos) 9) ) neighbours)))

(defun apply-step (dumbos)
  ;reset to 1 dumbos that have flashed and increase others by 1
  (loop for pos being the hash-keys in dumbos using (hash-value energy) do
          (if (> energy 9) (setf (gethash pos dumbos) 1) (incf (gethash pos dumbos))))

  ; loop for flashing dumbos, cascading as needed
  (loop for flashings = (loop for pos being the hash-keys in dumbos using (hash-value energy)
                              if (= energy 10)
                              collect pos)
        then new-flashings
        for new-flashings = nil
        for total-flashes = (length flashings) then (+ total-flashes (length flashings))
        while (> (length flashings) 0)
        finally (return total-flashes)
        do (loop for f in flashings do
                     (dolist (n (get-flashable-neighbours f dumbos))
                       (incf (gethash n dumbos))
                       (if (= (gethash n dumbos) 10) (push n new-flashings))))))

(defun part-1 (dumbos steps)
  (loop for step from 1 to steps
        sum (apply-step dumbos)))

(defun are-all-flashing (dumbos)
  (loop for energy being the hash-values in dumbos do
          (if (< energy 10) (return-from are-all-flashing nil)))
  t)

(defun part-2 (dumbos)
  (loop for step = 1 then (1+ step)
        until (are-all-flashing dumbos)
        do (apply-step dumbos)
        finally (return (1- step))))

(format t "~% answer 1: ~a" (part-1 (create-dumbos-map) *steps*))
(format t "~% answer 2: ~a" (part-2 (create-dumbos-map)))

(format T "~%")