(load "2-input.lisp")

(defun delimiter-newline (c) (char= c #\Linefeed) )
(defun delimiter-space (c) (char= c #\Space) )

; https://stackoverflow.com/questions/15393797/lisp-splitting-input-into-separate-strings
(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))




(let ((x 0) (z 0) (steps (my-split *input-2* :delimiterp 'delimiter-newline)))
  ;(format t "~A" steps)
  (dolist (step steps)
    ;(format t"~A, " step)
    (let* ((split-step (my-split step :delimiterp 'delimiter-space)) (direction (first split-step)) (distance (parse-integer (second split-step))))
      ;(format t " direction: ~s | " direction)
      (cond
        ((string= direction "forward") (setf x (+ x distance)))
        ((string= direction "up") (setf z (- z distance)))
        ((string= direction "down") (setf z (+ z distance))))))
  ;(format t "x: ~d " x)
  ;(format t "z: ~d " z)
  (format t "answer 1: ~d " (* x z)))



(let ((x 0) (z 0) (aim 0) (steps (my-split *input-2* :delimiterp 'delimiter-newline)))
  (dolist (step steps)
    ;(format t"~A, " step)
    (let* ((split-step (my-split step :delimiterp 'delimiter-space)) (direction (first split-step)) (distance (parse-integer (second split-step))))
      ;(format t " direction: ~s | " direction)
      (cond
        ((string= direction "forward") (setf x (+ x distance)) (setf z (+ z (* aim distance))))
        ((string= direction "up") (setf aim (- aim distance)))
        ((string= direction "down") (setf aim (+ aim distance))))))
  ;(format t "x: ~d " x)
  ;(format t "z: ~d " z)
  (format t "answer 2: ~d " (* x z)))

