(defun map (f a1 a2 len )  (setf (aref a2 (- len 1)) (f (aref a1 (- len 1)))) (cond ((= len 1)  0) (t  (map f a1 a2 (- len 1)))))
(defun double (a ) (* a 2))
(defun main ()  (defparameter array1 0) (setq array1 #(3 5 6 )) (defparameter array2 0) (setq array2 (make-array 3)) (map double array1 array2 3) (format t "~d~%" (aref array2 0)) (format t "~d~%" (aref array2 1)) (format t "~d~%" (aref array2 2)))
