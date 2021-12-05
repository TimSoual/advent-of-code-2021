(load "utils.lisp")
(load "5-input.lisp")

;; parse input

(defun parse-coords (coords)
  "parse a string like \"1,5\" and returns a list of coords (1 5) "
  (mapcar 'parse-integer (my-split coords :delimiterp 'delimiter-comma)))

(defun check-hz-vt-vent (coords1 coords2)
  (let ((x1 (first coords1))
        (y1 (second coords1))
        (x2 (first coords2))
        (y2 (second coords2)))
    (or (= x1 x2) (= y1 y2))))

(defun check-hz-vt-diag-vent (coords1 coords2)
  (let ((x1 (first coords1))
        (y1 (second coords1))
        (x2 (first coords2))
        (y2 (second coords2)))
    (or (= x1 x2) (= y1 y2) (= (abs (- x2 x1)) (abs (- y2 y1))))))

(defun parse-and-filter-vents (raw-input check-fn)
  (let ((filtered-vents nil))
    (dolist (line (my-split raw-input :delimiterp 'delimiter-newline))
      (let* ((parsed-line (my-split line :delimiterp 'delimiter-space))
             (coords1 (parse-coords (first parsed-line)))
             (coords2 (parse-coords (third parsed-line))))
        (if (funcall check-fn coords1 coords2)
            (push (list coords1 coords2) filtered-vents))))
    (reverse filtered-vents)))

(defvar *hv-vents* (parse-and-filter-vents *input-test-5* 'check-hz-vt-vent))
(defvar *hvd-vents* (parse-and-filter-vents *input-test-5* 'check-hz-vt-diag-vent))


;; answers

(defun create-empty-map (map-size)
  (loop for x from 0 to map-size
        collecting (loop for y from 0 to map-size collecting 0)))

(defun mark-vent (vent vmap)
  (let* ((coords1 (first vent))
         (coords2 (second vent))
         (x1 (first coords1))
         (y1 (second coords1))
         (x2 (first coords2))
         (y2 (second coords2)))
    (cond
     ;horizontal
     ((= y1 y2)
      (let ((startx (min x1 x2))
            (endx (max x1 x2)))
        (loop for x from startx to endx do
                (incf (nth x (nth y1 vmap)) 1))))
     ;vertical
     ((= x1 x2)
      (let ((starty (min y1 y2))
            (endy (max y1 y2)))
        (loop for y from starty to endy do
                (incf (nth x1 (nth y vmap)) 1))))
     ;diag 1 (left to right from top)
     ((= (- x2 x1) (- y2 y1))
      (let* ((start (if (< x1 x2) coords1 coords2))
             (end (if (< x1 x2) coords2 coords1))
             (startx (first start))
             (starty (second start))
             (endx (first end))
             (endy (second end)))
        (loop for x from startx to endx
              for y from starty to endy do
                (incf (nth x (nth y vmap))))))
     ;diag 2 (right to left from top)
     ((= (- x2 x1) (- y1 y2))
      (let* ((start (if (< x1 x2) coords2 coords1))
             (end (if (< x1 x2) coords1 coords2))
             (startx (first start))
             (starty (second start))
             (endx (first end))
             (endy (second end)))
        (loop for x from startx downto endx
              for y from starty to endy do
                (incf (nth x (nth y vmap)))))))))

(defun count-dangerous-vents (vmap)
  (loop for y from 0 to (- (length vmap) 1)
        sum
        (loop for x from 0 to (- (length vmap) 1)
              counting
              (> (nth x (nth y vmap)) 1))))

(defun get-answer (vents)
  (let* ((map-size (apply 'max (mapcar #'(lambda (v) (apply 'max (mapcar #'(lambda (point) (apply 'max point)) v))) vents)))
         (vent-map (create-empty-map map-size)))
    (dolist (vent vents)
      (mark-vent vent vent-map))
    (count-dangerous-vents vent-map)))

(format t "~%answer 1: ~a" (get-answer *hv-vents*))
(format t "~%answer 2: ~a" (get-answer *hvd-vents*))

(format t "~%")
