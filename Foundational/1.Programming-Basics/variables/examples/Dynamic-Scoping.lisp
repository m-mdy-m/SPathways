(setq x 10)  ; Global x

(defun func1 ()
    (let ((x 20))  ; Local x in func1
        (func2)))  ; Calls func2, which will use x

(defun func2 ()
    (print x))  ; x is dynamically bound to 20, not 10

(func1)  ; Output: 20