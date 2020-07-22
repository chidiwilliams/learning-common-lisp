(defun make-table () (list '*table*))

(defun associate (key records)
  (cond ((null records) nil)
        ((equal key (caar records)) (car records))
        (T (associate key (cdr records)))))

(defun lookup (keys table)
  (let ((subtable (associate (car keys)
                             (cdr table))))
    (if subtable
        (if (null (cdr keys))
            (cdr subtable)
            (lookup (cdr keys) subtable))
        nil)))

(defun insert! (keys value table)
  (let ((inner (associate (car keys)
                          (cdr table))))
    (if inner
        (if (null (cdr keys))
            (setf (cdr inner) value)
            (insert! (cdr keys) value inner))
        (if (null (cdr keys))
            (setf (cdr table)
                  (cons (cons (car keys) value)
                        (cdr table)))
            (progn
              (setf (cdr table)
                    (cons (list (car keys))
                          (cdr table)))
              (insert! (cdr keys)
                       value
                       (cadr table))))))
  'ok)
