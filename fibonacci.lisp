;;; Computes the value of the nth
;;; number in the Fibonacci sequence
(defun fibonacci (n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (T (do
        ((curr 1) (ppn 0) (pn 1) (i 1 (1+ i)))
        ((= i n) curr)
         (setq curr (+ ppn pn)
               ppn pn
               pn curr)))))
