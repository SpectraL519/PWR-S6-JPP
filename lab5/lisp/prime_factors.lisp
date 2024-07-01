(defun factorize (n divisor)
  (if (= n 1)
    (list)
    (if (= (mod n divisor) 0)
        (cons divisor (factorize (/ n divisor) divisor))
        (factorize n (1+ divisor)))))

(defun prime-factors (n)
  (if (<= n 1)
    (list)
    (factorize n 2)))


(defun print-prime-factors (n)
  (format t "Prime factors of ~d: ~a~%" n (prime-factors n)))


(defun main ()
  (print-prime-factors 17)
  (print-prime-factors 36))

(main)
