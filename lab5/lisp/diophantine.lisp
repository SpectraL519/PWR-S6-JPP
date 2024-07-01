(defun ext-gcd (a b)
  (if (= b 0)
      (values 1 0 a)
      (multiple-value-bind (x1 y1 gcd-ab)
          (ext-gcd b (mod a b))
        (let ((x y1)
              (y (- x1 (* y1 (truncate a b)))))
          (values x y gcd-ab)))))

(defun de (a b)
  (ext-gcd a b))


(defun main ()
  (let* ((a 30)
         (b 42))
    (multiple-value-bind (x y z)
        (de a b)
      (format t "Solution to the equation ~dx + ~dy = gcd(~d, ~d):~%" a b a b)
      (format t "x = ~d | y = ~d | gcd(~d, ~d) = ~d~%" x y a b z)
      (format t "Check: ~d * ~d + ~d * ~d = ~d~%" a x b y (+ (* a x) (* b y))))))

(main)
