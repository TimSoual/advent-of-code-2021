(load "3-input.lisp")


(defconstant INPUT-TEST-NUMS-DIGITS 5)
(defconstant INPUT-NUMS-DIGITS 12)

; https://stackoverflow.com/questions/15393797/lisp-splitting-input-into-separate-strings
(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defun delimiter-newline (c) (char= c #\Linefeed) )


(defun extract-columns (string nums-digit)
  (loop for digit from 0 to (- nums-digit 1)
    collect
        (do ((nums nil) (i digit (incf i (+ nums-digit 1))))
            ((>= i (length string)) (nreverse nums))
          (push (char string i) nums))))


(defun most-common (list)
  "returns the most common digit in the list"
  (let ((ht (make-hash-table)))
    (setf (gethash #\1 ht) 0)
    (setf (gethash #\0 ht) 0)
    (dolist (value list)
      (incf (gethash value ht) 1))
    (if (>= (gethash #\1 ht) (gethash #\0 ht))
      "1"
      "0")))

(defun least-common (list)
  "returns the least common digit in the list"
  (let ((ht (make-hash-table)))
    (setf (gethash #\1 ht) 0)
    (setf (gethash #\0 ht) 0)
    (dolist (value list)
      (incf (gethash value ht) 1))
    (if (< (gethash #\1 ht) (gethash #\0 ht))
      "1"
      "0")))





(let ((columns (extract-columns INPUT INPUT-NUMS-DIGITS)))
  (format t "answer 1: ~a"
    (*
      (parse-integer
        ( apply #'concatenate 'string
          (mapcar #'most-common columns))
        :radix 2)
      (parse-integer
        ( apply #'concatenate 'string
          (mapcar #'least-common columns))
        :radix 2))))

(defun most-common-in-column (values column)
  "find most common digit for given column in a list of values"
  (most-common (mapcar #'(lambda (x) (char x column)) values)))

(defun least-common-in-column (values column)
  "find least common digit for given column in a list of values"
  (least-common (mapcar #'(lambda (x) (char x column)) values)))


(let* ((columns (extract-columns INPUT INPUT-NUMS-DIGITS))
       (numbers-og-rating (my-split INPUT :delimiterp 'delimiter-newline))
       (numbers-sc-rating numbers-og-rating))
  (loop for i from 0 to (- INPUT-NUMS-DIGITS 1)
      for mcv = (most-common-in-column numbers-og-rating i)
      never (< (length numbers-og-rating) 2)
      do (setf numbers-og-rating
      (remove-if-not #'(lambda (x) (char= (char mcv 0) (char x i))) numbers-og-rating))
      finally (return numbers-og-rating))
  ;(format t "~% og rating: ~a" (first numbers-og-rating))

  (loop for i from 0 to (- INPUT-NUMS-DIGITS 1)
      for mcv = (least-common-in-column numbers-sc-rating i)
      never (< (length numbers-sc-rating) 2)
      do (setf numbers-sc-rating
      (remove-if-not #'(lambda (x) (char= (char mcv 0) (char x i))) numbers-sc-rating))
      finally (return numbers-sc-rating))
  ;(format t "~% sc rating: ~a" (first numbers-sc-rating))

  (format t "~%answer 2: ~A"
      (*
      (parse-integer (first numbers-og-rating) :radix 2)
      (parse-integer (first numbers-sc-rating) :radix 2))))


