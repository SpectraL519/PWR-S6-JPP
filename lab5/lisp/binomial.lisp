(defun binomial (n k)
  (cond ((> k n) 0)
        ((or (= k 0) (= k n)) 1)
        (t (+ (binomial (- n 1) k) (binomial (- n 1) (- k 1))))))


(defun add-element-wise (l1 l2)
  (cond ((and (null l1) (null l2)) nil)
        ((null l1) l2)
        ((null l2) l1)
        (t (cons (+ (car l1) (car l2))
                 (add-element-wise (cdr l1) (cdr l2))))))

(defun pascals-triangle (n)
  (if (= n 0)
      '(1)
      (let ((prev-row (pascals-triangle (- n 1))))
        (add-element-wise (cons 0 prev-row) (append prev-row '(0))))))

(defun binomial2 (n k)
  (nth k (pascals-triangle n)))


(defun time-it (fn)
  (let ((start (get-internal-real-time)))
    (let ((result (funcall fn)))
      (let ((end (get-internal-real-time)))
        (values result (/ (- end start) internal-time-units-per-second))))))


(defun main ()
  (let ((n 28)
        (k 14))
    (multiple-value-bind (result1 time1) (time-it (lambda () (binomial n k)))
      (format t "binomial ~d ~d = ~d~%" n k result1)
      (format t "Time for binomial ~d ~d: ~f seconds~%" n k time1))
    (format t "~%")
    (multiple-value-bind (result2 time2) (time-it (lambda () (binomial2 n k)))
      (format t "binomial2 ~d ~d = ~d~%" n k result2)
      (format t "Time for binomial2 ~d ~d: ~f seconds~%" n k time2))))

(main)
