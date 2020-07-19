(defun make-table ()
  (list '*table*))

(defun associate (key records)
  (cond ((null records) nil)
        ((equal key (caar records)) (car records))
        (T (associate key (cdr records)))))

(defun lookup (key-1 key-2 table)
  (let ((subtable (associate key-1 (cdr table))))
    (if subtable
        (let ((record
                (associate key-2 (cdr subtable))))
          (if record
              (cdr record)
              nil))
        nil)))

(defun insert! (key-1 key-2 value table)
  (let ((subtable (associate key-1 (cdr table))))
    (if subtable
        (let ((record
                (associate key-2 (cdr subtable))))
          (if record
              (setf (cdr record) value)
              (setf (cdr subtable)
                    (cons (cons key-2 value)
                          (cdr subtable)))))
        (setf (cdr table)
              (cons (list key-1
                          (cons key-2 value))
                    (cdr table)))))
  'ok)
