(load "utils.lisp")
(load "14-input.lisp")

(defvar *template* nil)
(defvar *rules* nil)

(defun parse-input (inpt)

  (let ((lines (my-split-2 inpt :delimiterp 'delimiter-newline)))
    (setf *template* (first lines))
    (setf *rules*
          (mapcar
           #'(lambda (x) (list (first x) (third x)))
           (loop for l in (cddr lines)
                 collect
                 (my-split l :delimiterp 'delimiter-space))))))

(parse-input *input-14*)

(defun apply-step (polymer rules)
  (loop for i from 0 to (- (length polymer) 2)
        for substr = (subseq polymer i (+ i 2))
        for s1 = (subseq polymer i (1+ i))
        with new-poly = ""
        do (setf new-poly (concatenate
                           'string
                           new-poly
                           (loop for (between inserted) in rules
                                 with new-str = s1
                                 do (when (string= substr between)
                                          (setf new-str (concatenate 'string new-str inserted))
                                          (return new-str))
                                 finally (return new-str))))
        finally (return (concatenate 'string new-poly (subseq polymer (1- (length polymer)) (length polymer))))))

(defun apply-steps (template rules steps)
  (loop for step from 0 to steps
        for polymer = template then (apply-step polymer rules)
        finally (return polymer)))

(defun calc-polymer-score (polymer)
  (let ((atoms (loop for i from (char-code #\A) to (char-code #\Z)
                     for chr = (code-char i)
                     if (position chr polymer)
                     collect chr)))
    (apply '- (loop for chr in atoms
                    for
                    count = (loop for c across polymer count (char= c chr))
                    maximize
                    count into cmax
                    minimize
                    count into cmin
                    finally (return (list cmax cmin))))))

(defun part-1 (steps)
  (let ((polymer (apply-steps *template* *rules* steps)))
    (calc-polymer-score polymer)))



;;;; Restart a whole new approach for part 2 as the naive approach of part 1 was too inefficient for 40 steps.


(defun create-polymer-ht (template)
  (let ((polymer-ht (make-hash-table)))
    (loop for i from 0 to (- (length template) 2) do
            (if (gethash (subseq template i (+ i 2)) polymer-ht)
                (incf (gethash (subseq template i (+ i 2)) polymer-ht))
                (setf (gethash (subseq template i (+ i 2)) polymer-ht) 1)))
    polymer-ht))

(defun create-rules-ht (rules)
  (loop for (between inserted) in rules
        with rules-ht = (make-hash-table :test 'equal)
        do (setf (gethash between rules-ht)
                 (list
                  (concatenate 'string (subseq between 0 1) inserted)
                  (concatenate 'string inserted (subseq between 1))))
        finally (return rules-ht)))

(defun apply-step-2 (polymer-ht rules-ht)
  (loop for quantity being the hash-values of polymer-ht using (hash-key old)
        for (new1 new2) = (gethash old rules-ht)
        with new-polymer-ht = (make-hash-table :test 'equal)
        do (if (gethash new1 new-polymer-ht)
               (incf (gethash new1 new-polymer-ht) quantity)
               (setf (gethash new1 new-polymer-ht) quantity))
           (if (gethash new2 new-polymer-ht)
               (incf (gethash new2 new-polymer-ht) quantity)
               (setf (gethash new2 new-polymer-ht) quantity))
        finally (return new-polymer-ht)))

(defun apply-steps-2 (polymer-ht rules-ht steps)
  (loop for step from 0 to steps
        for new-polymer-ht = polymer-ht then (apply-step-2 new-polymer-ht rules-ht)
        finally (return new-polymer-ht)))

(defun calc-polymer-score-2 (polymer-ht template)
  (let ((letters-ht (make-hash-table :test 'equal)))
    (loop for pair being the hash-keys in polymer-ht using (hash-value quantity)
          for c0 = (char pair 0)
          for c1 = (char pair 1) do
            (if (gethash c0 letters-ht)
                (incf (gethash c0 letters-ht) quantity)
                (setf (gethash c0 letters-ht) quantity))

            (if (gethash c1 letters-ht)
                (incf (gethash c1 letters-ht) quantity)
                (setf (gethash c1 letters-ht) quantity)))

    ; Characters at the beginning and end are only counted once in polymer-ht, others are counted twice.
    ; So we add 1 to beginning and end before dividing everything by 2 and counting.
    (incf (gethash (char template 0) letters-ht))
    (incf (gethash (char template (1- (length template))) letters-ht))
    (apply '-
           (loop for letter being the hash-keys in letters-ht using (hash-value cnt)
                 for real-cnt = (/ cnt 2)
                 maximize real-cnt into cmax
                 minimize real-cnt into cmin
                 finally (return (list cmax cmin))))))

(defun part-2 (steps)
  (calc-polymer-score-2
   (apply-steps-2
    (create-polymer-ht *template*)
    (create-rules-ht *rules*)
    steps)
   *template*))

(format t "~%answer 1: ~a" (part-2 10)) ;could also use (part-1 10) but it takes more time for the same result.
(format t "~%answer 2: ~a" (part-2 40))

(format T "~%")
