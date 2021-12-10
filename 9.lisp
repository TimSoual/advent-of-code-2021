(load "utils.lisp")
(load "9-input.lisp")

(defvar *vents* (loop for line in (my-split *input-9* :delimiterp 'delimiter-newline)
                      collecting
                      (loop for charac across line collecting (digit-char-p charac))))

;input x size and y size
(defvar *last-i* (1- (length *vents*)))
(defvar *last-j* (1- (length (first *vents*))))

(defun is-low-point (vent neighbours)
  (= (loop for n in neighbours count (>= vent n)) 0))

(defun vent-at (i j vents-map)
  (nth j (nth i vents-map)))

(defun part-1 (vents-map)
  (loop for i from 0 to (1- (length vents-map))
        for line = (nth i vents-map)
        sum
        (loop for j from 0 to (1- (length line))
              for vent = (nth j line)
              sum
              (if (is-low-point vent
                                (let ((neighbours nil))
                                  (if (/= i 0) (push (vent-at (1- i) j vents-map) neighbours))
                                  (if (/= i *last-i*) (push (vent-at (1+ i) j vents-map) neighbours))
                                  (if (/= j 0) (push (vent-at i (1- j) vents-map) neighbours))
                                  (if (/= j *last-j*) (push (vent-at i (1+ j) vents-map) neighbours))
                                  neighbours))
                  (1+ vent)
                  0))))

(defun find-non-9-neighbours (position vents-map)
  (let ((i (realpart position))
        (j (imagpart position))
        (neighbours nil))
    (if (and (/= i 0) (/= (vent-at (1- i) j vents-map) 9)) (push (complex (1- i) j) neighbours))
    (if (and (/= i *last-i*) (/= (vent-at (1+ i) j vents-map) 9)) (push (complex (1+ i) j) neighbours))
    (if (and (/= j 0) (/= (vent-at i (1- j) vents-map) 9)) (push (complex i (1- j)) neighbours))
    (if (and (/= j *last-j*) (/= (vent-at i (1+ j) vents-map) 9)) (push (complex i (1+ j)) neighbours))
    neighbours))

(defun find-all-neighbours (positions basin vents-map)
    "returns a list of the positions that are neighbour to the given positions in vents-map and not already in basin"
  (remove-duplicates (loop for p in positions
                           append
                           (loop for neighbour in (find-non-9-neighbours p vents-map)
                                 if (not (position-if #'(lambda (bpos) (= bpos neighbour)) basin))
                                 collect neighbour))))

(defun calculate-basin-new (pos vents-map)
    "returns a list of positions representing a newly calculated basin"
  (let ((basin nil))
    (push pos basin)
    ;While there remains neighbours, add them to basin and seek their neighbours, etc
    (do ((neighbours (find-non-9-neighbours pos vents-map) (find-all-neighbours neighbours basin vents-map)))
        ((= (length neighbours) 0) basin)
        (setf basin (concatenate 'list basin neighbours)))
    basin))

(defun part-2 (vents-map)
  (let ((basins nil))
    (loop for i from 0 to (1- (length vents-map))
          for line = (nth i vents-map) do
            (loop for j from 0 to (1- (length line))
                  for vent = (nth j line)
                  for pos = (complex i j) do
                    ;if vent value not 9 and pos not already in a basin, calculate a new basin
                    (unless (or (= vent 9) (> (loop for basin in basins count (position-if #'(lambda (b) (= b pos)) basin)) 0))
                            (push (calculate-basin-new pos vents-map) basins))))

    (let ((blengths (sort (mapcar 'length basins) '>)))
      (* (first blengths) (second blengths) (third blengths)))))

(format t "~% answer 1: ~a" (part-1 *vents*))
(format t "~% answer 1: ~a" (part-2 *vents*))

(format T "~%")
