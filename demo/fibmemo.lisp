(defparameter fibarray 0) (setq fibarray (make-array 40))
(defun init_array (n x )  (setf (aref fibarray (- n 1)) x) (cond ((= n 1)  0) (t  (init_array (- n 1) x))))
(defun fib (n )  (let (f ) (cond ((= (aref fibarray n) -1)  (setq f (+ (fib (- n 1)) (fib (- n 2)))) (setf (aref fibarray n) f) f) (t  (aref fibarray n)))))
(defun main ()  (init_array 40 -1) (setf (aref fibarray 0) 0) (setf (aref fibarray 1) 1) (format t "~d~%" (fib (read))))
