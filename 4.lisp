(load "utils.lisp")
(load "4-input.lisp")

(defvar *numbers*
  (mapcar
   'parse-integer
   (my-split
    (first (my-split *input-4* :delimiterp 'delimiter-newline))
    :delimiterp 'delimiter-comma)))

(defvar *boards*
  (let ((boards nil)
        (current-board nil))
    (dolist (el (cddr (my-split-2 *input-4* :delimiterp 'delimiter-newline)))
      (if (< (length el) 1)
          (progn
           (push (reverse current-board) boards)
           (setf current-board nil))
          (progn
           (push
            (mapcar 'parse-integer
                    (my-split el :delimiterp 'delimiter-space))
            current-board))))
    (push (reverse current-board) boards)
    (reverse boards)))

(defvar *marking-boards* (mapcar
                          #'(lambda (board)
                            (mapcar #'(lambda (line) (mapcar #'(lambda (el) 0 ) line)) board))
                          *boards*))

(defun print-board (board)
  (format t "~%board: ~%~{~a~^~%~}~%-" board))

(defun print-boards (boards)
  (format t "~%boards: ~%~{~{~a~^~%~}~^,~%~% ~}~%---~%" boards))

(defun mark-boards (number)
  (loop for board in *boards*
        for mboard in *marking-boards*
        collect (loop for line in board
                      for mline in mboard
                      collect (loop for el in line
                                    for mel in mline
                                    collect (if (or (= mel 1) (= number el)) 1 0)))))

(defun check-win-condition (board)
  (or
   ;lines
   (> (loop for line in board counting (= (apply '+ line) (length line))) 0)
   ;columns
   (> (loop for i from 0 to (- (length board) 1) counting (= (apply '+ (mapcar #'(lambda (line) (nth i line) ) board)) (length board))) 0)))

(defun sum-unmarked-numbers (board mboard)
  (loop for line in board
        for mline in mboard
        sum (loop for el in line
                  for mel in mline
                  sum (if (= mel 0) el 0))))

(defun check-all-and-maybe-end ()
  (dotimes (bindex (length *boards*))
    (if (check-win-condition (nth bindex *marking-boards*))
        (return-from check-all-and-maybe-end
                     (sum-unmarked-numbers (nth bindex *boards*) (nth bindex *marking-boards*))))))

(dolist (num *numbers*)
  (setf *marking-boards* (mark-boards num))
  (let ((winning-sum (check-all-and-maybe-end)))
    (when winning-sum
          (format t "~%answer 1: ~a" (* winning-sum num))
          (return))))


; *marking-boards* is not reinitialised between answers, but it doesn't change much
(let ((board-won-list nil))
  (dolist (num *numbers*)
    (setf *marking-boards* (mark-boards num))
    (dotimes (bindex (length *boards*))
      (if (and (check-win-condition (nth bindex *marking-boards*)) (not (position bindex board-won-list)))
          (push bindex board-won-list)))
    (if (= (length board-won-list) (length *boards*))
        (let ((last-won-index (first board-won-list)))
          (format t "~%answer 1: ~a"
                  (* (sum-unmarked-numbers
                      (nth last-won-index *boards*)
                      (nth last-won-index *marking-boards*))
                     num))
          (return)))))

(format t "~%")
