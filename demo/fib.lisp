(defun fib (n ) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(defun main ()  (format t "~d~%" (fib (read))))
