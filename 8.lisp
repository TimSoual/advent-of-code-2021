(load "utils.lisp")
(load "8-input.lisp")

(defvar *entries* (my-split *input-8* :delimiterp 'delimiter-newline))




(defun part-1 (entries)
    (loop for entry in entries
        for entry-input = (first (my-split entry :delimiterp 'delimiter-pipe))
        for entry-output = (second (my-split entry :delimiterp 'delimiter-pipe))
        ; do (println entry-ouput)
        sum
        (loop for value in (my-split entry-output :delimiterp 'delimiter-space)
            for len = (length value) count
            (or (= len 2) (= len 3) (= len 4) (= len 7)))))

(defun create-digit-by-segments ()
    (let ((digit-by-segments (make-hash-table :test 'equal)))
        ;; setting digits values to decode output
        (setf (gethash "abcefg" digit-by-segments) 0)
        (setf (gethash "cf" digit-by-segments) 1)
        (setf (gethash "acdeg" digit-by-segments) 2)
        (setf (gethash "acdfg" digit-by-segments) 3)
        (setf (gethash "bcdf" digit-by-segments) 4)
        (setf (gethash "abdfg" digit-by-segments) 5)
        (setf (gethash "abdefg" digit-by-segments) 6)
        (setf (gethash "acf" digit-by-segments) 7)
        (setf (gethash "abcdefg" digit-by-segments) 8)
        (setf (gethash "abcdfg" digit-by-segments) 9)
        digit-by-segments))

(defun create-real-segment-by-wrong-segment (wsegments-occurences l2 l4)
    (let ((real-segment-by-wrong-segment (make-hash-table)))
        ;a: 8 occurences with none in l2
        (setf (gethash (first (find-if #'(lambda (el) (and (= (second el) 8) (= (count (first el) l2) 0) )) wsegments-occurences)) real-segment-by-wrong-segment) #\a )
        ;b: 6 occurences
        (setf (gethash (first (find-if #'(lambda (el) (= (second el) 6)) wsegments-occurences)) real-segment-by-wrong-segment)  #\b)
        ;c: 8 occurences with one in l2
        (setf (gethash (first (find-if #'(lambda (el) (and (= (second el) 8) (= (count (first el) l2) 1) )) wsegments-occurences)) real-segment-by-wrong-segment) #\c )
        ;d: 7 occurences with one in l4
        (setf (gethash (first (find-if #'(lambda (el) (and (= (second el) 7) (= (count (first el) l4) 1) )) wsegments-occurences)) real-segment-by-wrong-segment) #\d )
        ;e: 4 occurences
        (setf (gethash (first (find-if #'(lambda (el) (= (second el) 4)) wsegments-occurences)) real-segment-by-wrong-segment)  #\e)
        ;f: 9 occurences
        (setf (gethash (first (find-if #'(lambda (el) (= (second el) 9)) wsegments-occurences)) real-segment-by-wrong-segment)  #\f)
        ;g : 7 occurences and not in l4
        (setf (gethash (first (find-if #'(lambda (el) (and (= (second el) 7) (= (count (first el) l4) 0) )) wsegments-occurences)) real-segment-by-wrong-segment) #\g )

        real-segment-by-wrong-segment))


(defun decode-and-sum-values (entry-input entry-output)
    (let*
        (
        (digit-by-segments (create-digit-by-segments))
        (input-values-and-lengths (mapcar #'(lambda (v) (list v (length v))) (my-split entry-input :delimiterp 'delimiter-space)))
        (l2 (first (find-if #'(lambda (el) (= (second el) 2)) input-values-and-lengths))) ;input value with length 2
        (l4 (first (find-if #'(lambda (el) (= (second el) 4)) input-values-and-lengths))) ;input value with length 4
        (wsegments-occurences (loop for wseg in '(#\a #\b #\c #\d #\e #\f #\g) collect
             (list wseg (count wseg entry-input) )))
        (real-segment-by-wrong-segment (create-real-segment-by-wrong-segment wsegments-occurences l2 l4))
        )

        (parse-integer (format nil "~{~A~}" (loop for wrong-val in (my-split entry-output :delimiterp 'delimiter-space)
            for real-val = (format nil "~{~A~}" (sort (loop for character across wrong-val collecting (gethash character real-segment-by-wrong-segment)) 'char<))
            collect (gethash real-val digit-by-segments))))))


(defun part-2 (entries)
    (loop for entry in entries
        for entry-input = (first (my-split entry :delimiterp 'delimiter-pipe))
        for entry-output = (second (my-split entry :delimiterp 'delimiter-pipe))
        sum
        (decode-and-sum-values entry-input entry-output)))


(format T "~% answer 1: ~a" (part-1 *entries*))
(format T "~% answer 2: ~a" (part-2 *entries*))

(format T "~%")
