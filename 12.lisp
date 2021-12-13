(load "utils.lisp")
(load "12-input.lisp")

(defun parse-input (input)
  "returns a nice hastable with caves and neighbour caves"
  (loop for entry in (my-split input :delimiterp 'delimiter-newline)
        for (cave neighbour) = (my-split entry :delimiterp 'delimiter-dash)
        with ht = (make-hash-table :test 'equal)
        do (progn
            (if (gethash cave ht) (push neighbour (gethash cave ht)) (setf (gethash cave ht) (list neighbour)))
            (if (gethash neighbour ht) (push cave (gethash neighbour ht)) (setf (gethash neighbour ht) (list cave))))
        finally (return ht)))

(defvar *caves-table* (parse-input *input-12*))

(defun is-small-cave (cave)
  "test if small cave: small caves have no capital letters"
  (loop for c across cave
        for code = (char-code c)
        never (and (>= code 65) (<= code 90))))

(defun find-path (caves-table visited current conditionp)
  "recursively search for paths"
  (when (string= current "end")
        (return-from find-path (list (push "end" visited))))

  (when (funcall conditionp current visited)
        (return-from find-path nil))

  (push current visited)

  (let ((neighbours (gethash current caves-table))
        (paths nil))
    (loop for neighbour in neighbours do
            (setf paths (concatenate 'list (find-path caves-table visited neighbour conditionp) paths)))

    (remove nil paths)))

(defun part-1-stopp (current visited)
  "part 1: can only visit small caves once"
  (and (is-small-cave current) (position current visited :test 'equal)))

(defun part-2-stopp (current visited)
  "part 2: can only visit a single small cave twice except start, and other small caves only once"
  (let* ((visited-small-caves (remove-if-not 'is-small-cave visited))
         (seen-small-cave-twice? (/= (length visited-small-caves) (length (remove-duplicates visited-small-caves :test 'equal)))))
    (and (is-small-cave current)
         (or
          (and (string= current "start") (position current visited :test 'equal))
          (and seen-small-cave-twice? (position current visited :test 'equal))
          (and (not seen-small-cave-twice?) (> (count current visited :test 'equal) 1))))))

(defun part-1 (caves-table)
  (length (find-path caves-table nil "start" 'part-1-stopp)))

(defun part-2 (caves-table)
  (length (find-path caves-table nil "start" 'part-2-stopp)))

(format t "~% answer 1: ~a" (part-1 *caves-table*))
(format t "~% answer 2: ~a" (part-2 *caves-table*))

(format T "~%")
