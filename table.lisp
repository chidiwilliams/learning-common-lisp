(defun make-table ()
  (list '*table*))

(defun lookup (key table)
  (let ((record (associate key (cdr table))))
    (if record
        (cdr record)
        nil)))

(defun insert! (key value table)
  (let ((record (associate key (cdr table))))
    (if record
        (setf (cdr record) value)
        (setf (cdr table)
              (cons (cons key value)
                    (cdr table))))))

(defun associate (key records)
  (cond ((null records) nil)
        ((equal key (caar records)) (car records))
        (T (associate key (cdr records)))))
