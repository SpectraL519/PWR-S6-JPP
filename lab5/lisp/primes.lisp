(defun number-sequence (start end)
  (if (> start end)
      nil
      (cons start (number-sequence (1+ start) end))))

(defun primes (n)
  (labels ((sieve (lst)
             (cond ((null lst) nil)
                   ((> (* (car lst) (car lst)) n) lst)
                   (t (cons (car lst)
                            (sieve (remove-if (lambda (x) (zerop (mod x (car lst))))
                                              (cdr lst))))))))
    (if (< n 2)
        nil
        (sieve (number-sequence 2 n)))))


(defun main ()
  (let ((n 100))
    (format t "Prime numbers up to ~d: ~a~%" n (primes n))))

(main)
