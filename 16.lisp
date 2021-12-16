(load "utils.lisp")
(load "16-input.lisp")

(defvar *npt* *input-16*)

(defvar *input-binary* (loop for c across *npt*
			     with input-binary = ""
			     do (setf input-binary
				      (concatenate 'string input-binary (format nil "~4,'0b" (digit-char-p c 16))))
			     finally (return input-binary)))

(defvar *versions-sum* 0)


(defun parse-literal-value (str)
  "parse literal value content of packet and return a list of end index and value"
  (loop for i = 6 then (+ i 5)
	with value-str = ""
	do (setf value-str (concatenate 'string value-str (subseq str (1+ i) (+ i 5) )))
	if (char= (char str i) #\0)
	  do (return (list (+ i 4) (parse-integer value-str :radix 2)))))

(defun get-operation (type)
  (cond ((= type 0) '+)
	((= type 1) '*)
	((= type 2) 'min)
	((= type 3) 'max)
	((= type 5) #'(lambda (x y) (if (> x y) 1 0)))
        ((= type 6) #'(lambda (x y) (if (< x y) 1 0)))
        ((= type 7) #'(lambda (x y) (if (= x y) 1 0)))))

(defun parse-operator (substr)
  "recursively parse operator string and return a list of end index and return value"
  (let ((type (parse-integer (subseq substr 3 6) :radix 2))
	(indicator (digit-char-p (char substr 6))))
    (cond ((= indicator 0)
	   (let* ((sub-packets-len (parse-integer (subseq substr 7 22) :radix 2))
		  (sub-packets-end (+ 22 sub-packets-len)))  
	     (loop for start = 22 then (+ start end 1)
		   with values = nil
		   while (< start sub-packets-end)
		   for (end value) = (parse-packet (subseq substr start sub-packets-end))
		   do (push value values)
		   finally (return (list (1- start) (apply (get-operation type) (reverse values)))))))
	  ((= indicator 1)
	   (let ((packets-count (parse-integer (subseq substr 7 18) :radix 2)))
	     (loop for i from 1 to packets-count
		   with values = nil
		   for start = 18 then (+ start end 1)
		   for (end value) = (parse-packet (subseq substr start))
		   do (push value values)
		   finally (return (list (+ start end) (apply (get-operation type) (reverse values))))))))))

(defun parse-packet (str)
  (let* ((version (parse-integer (subseq str 0 3) :radix 2))
	 (type (parse-integer (subseq str 3 6) :radix 2)))
    (incf *versions-sum* version)
    (if (= type 4)
	(parse-literal-value str)
	(parse-operator str))))


(defun part-1 ()
  (parse-packet *input-binary*)
  *versions-sum*)

(defun part-2 ()
  (second (parse-packet *input-binary*)))


(format t "~%answer 1: ~a" (part-1))
(format t "~%answer 2: ~a" (part-2))

(format T "~%")
