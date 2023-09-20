(defun fib (n ) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(defun fact (n ) (if (= n 1) n (* n (fact (- n 1)))))
(defun gcm (n1 n2 )  (let (ret ) (setq ret (mod n1 n2)) (if (= ret 0) n2 (gcm n2 ret))))
(defun main ()  (format t "~d~%" (fib (read))) (format t "~d~%" (fact (read))) (format t "~d~%" (gcm (read) (read))))
