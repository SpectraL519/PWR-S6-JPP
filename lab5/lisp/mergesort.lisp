(defun merge-lists (left right)
  (cond
    ((null left) right)
    ((null right) left)
    ((<= (car left) (car right))
     (cons (car left) (merge-lists (cdr left) right)))
    (t
     (cons (car right) (merge-lists left (cdr right))))))


(defun merge-sort (lst)
  (if (<= (length lst) 1)
      lst
      (let* ((mid (truncate (length lst) 2))
             (left (subseq lst 0 mid))
             (right (subseq lst mid)))
        (merge-lists (merge-sort left) (merge-sort right)))))


(defun main ()
  (let* ((unsorted-list '(3 1 4 1 5 9 2 6 5 3 5))
         (sorted-list (merge-sort unsorted-list)))
    (format t "Unsorted list: ~a~%" unsorted-list)
    (format t "Sorted list: ~a~%" sorted-list)))

(main)
