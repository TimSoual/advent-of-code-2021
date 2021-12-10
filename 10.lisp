(load "utils.lisp")
(load "10-input.lisp")

(defvar *lines* (my-split *input-10* :delimiterp 'delimiter-newline))

(defun find-illegal (lines)
  (loop for line in lines
        collect
        (loop for c across line
              with open-symbols = nil
              do (cond
                  ((char= c (code-char 41)) ;using code-char cause closed paren destroys vscode
                   (if (char= (first open-symbols) (code-char 40)) (pop open-symbols) (return c)))
                  ((char= c #\])
                   (if (char= (first open-symbols) #\[) (pop open-symbols) (return c)))
                  ((char= c #\})
                   (if (char= (first open-symbols) #\{) (pop open-symbols) (return c)))
                  ((char= c #\>)
                   (if (char= (first open-symbols) #\<) (pop open-symbols) (return c)))
                  ;other characters
                  (T (push c open-symbols))))))

(defun get-char-score (character)
  (cond
   ((not character) 0)
   ((char= character (code-char 41)) 3)
   ((char= character #\]) 57)
   ((char= character #\}) 1197)
   ((char= character #\>) 25137)))

(defun part-1 (lines)
  (apply '+ (mapcar 'get-char-score
                    (find-illegal lines))))


;;;;; part 2

(defun find-open-chars (lines)
  (loop for line in lines
        collect
        (loop for c across line
              with open-symbols = nil
              do (cond
                  ((char= c (code-char 41)) ;using code-char cause closed paren destroys vscode
                   (if (char= (first open-symbols) (code-char 40)) (pop open-symbols) (return nil)))
                  ((char= c #\])
                   (if (char= (first open-symbols) #\[) (pop open-symbols) (return nil)))
                  ((char= c #\})
                   (if (char= (first open-symbols) #\{) (pop open-symbols) (return nil)))
                  ((char= c #\>)
                   (if (char= (first open-symbols) #\<) (pop open-symbols) (return nil)))
                  ;other characters
                  (T (push c open-symbols)))
              finally (return open-symbols))))

(defun get-char-score-2 (character)
  (cond
   ((char= character (code-char 40)) 1)
   ((char= character #\[) 2)
   ((char= character #\{) 3)
   ((char= character #\<) 4)))

(defun get-score (open-chars)
  (loop for line in (remove nil open-chars)
        collect
        (loop for c in line
              with score = 0
              do (setf score (+ (* score 5) (get-char-score-2 c)))
              finally (return score))))

(defun part-2 (lines)
  (let ((scores (sort (get-score (find-open-chars lines)) '<)))
    (nth (floor (/ (length scores) 2)) scores)))


(format t "~% answer 1: ~a" (part-1 *lines*))
(format t "~% answer 2: ~a" (part-2 *lines*))

(format T "~%")
