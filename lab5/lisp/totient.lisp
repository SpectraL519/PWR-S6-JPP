(defun coprime (n)
  (let (result)
    (dotimes (k n (nreverse result))
      (let ((number (1+ k)))
        (when (= (gcd n number) 1)
          (push number result))))))

(defun totient (n)
  (length (coprime n)))


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

; results in stack overflow even for small prime numbers like 7919
(defun totient2 (n)
  (let* ((factors (prime-factors n))
         (unique-factors (remove-duplicates factors :test #'equal)))
    (round (* n (reduce #'* (mapcar (lambda (p) (- 1 (/ 1.0 p))) unique-factors))))))


(defun time-it (fn)
  (let ((start (get-internal-real-time)))
    (let ((result (funcall fn)))
      (let ((end (get-internal-real-time)))
        (values result (/ (- end start) internal-time-units-per-second))))))


(defun main ()
  (let ((n 1000000))
    (multiple-value-bind (result1 time1) (time-it (lambda () (totient n)))
      (format t "totient ~d = ~d~%" n result1)
      (format t "Time for totient ~d: ~f seconds~%" n time1))
    (format t "~%")
    (multiple-value-bind (result2 time2) (time-it (lambda () (totient2 n)))
      (format t "totient2 ~d = ~d~%" n result2)
      (format t "Time for totient2 ~d: ~f seconds~%" n time2))))

(main)
