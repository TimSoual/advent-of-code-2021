; https://stackoverflow.com/questions/15393797/lisp-splitting-input-into-separate-strings
(defun my-split (string &key (delimiterp 'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defun delimiter-newline (c) (char= c #\Linefeed) )
(defun delimiter-space (c) (char= c #\Space) )
(defun delimiter-comma (c) (char= c #\,) )
(defun delimiter-pipe (c) (char= c #\|) )

; my-split does not produce an empty string for 2 delimiters side by side, this does
(defun my-split-2 (string &key (delimiterp 'delimiterp))
  (let ((cur-string string) (split-strings nil))
    (do 
        ((beg 0)
        (end (or (position-if delimiterp cur-string ) (- (length cur-string) 1)) (or (position-if delimiterp cur-string ) (- (length cur-string) 1))))
      ((= (length cur-string) 0) )
      (if (= end (- (length cur-string) 1))
        (progn
            (push cur-string split-strings)
            (setf cur-string ""))
        (progn
            (push (subseq cur-string beg end) split-strings)
            (setf cur-string (subseq cur-string (+ end 1)))))
    )
    (reverse split-strings)))

(defun find-index (value comparator list)
  (dotimes (i (length list))
    (if (funcall comparator (nth i list) value) (return-from find-index i))))

(defun println (message) (format t "~%~a" message))
