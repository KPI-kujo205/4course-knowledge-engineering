;; simple hello world

(write-line "Hello world")

;; comments

;; this is my first comment

#|

this are multi line comments

|#

;; functions
(defun add (a b)
  (+ a b)
)  

(defun fib (n)
  "Return the nth Fibonacci number."
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(print (add 1 2))

(write-line "fibonacci output")

(print (fib 10))

;; can be called indirectly

(print (funcall #'fib 20))

;; functions can return multiple values

(defun many (n)
    (values n (* n 2) (* n 3)))

;; here is how to print a list of multiple values
(print (multiple-value-list (many 2)))

;; here is how to grab on1 value
(print (nth-value 1 (many 2)))

;; and this is possible as well
(multiple-value-bind (first second third)
             (many 2)

(print (list first second third)))


;; variables, created using let (()) synthax

(

let ((str "Hello, world!"))

(print (string-upcase str))

)

;; can also create multiple values at once

(
    let (
        (x 1)
        
        (y 5)

 
    )
    (print (+ x y))   
)

;; global variables

(defparameter *string* "I'm global")

(defun print-variable ()
  (print *string*))

(print-variable) ;; Prints "I'm global"

(let ((*string* "I have dynamic extent")) ;; Binds *string* to a new value
  (print-variable)) ;; Prints "I have dynamic extent"
;; The old value is restored

(print-variable) ;; Prints "I'm global"




;; lists

(print (second (list 1 2 3)))


(defparameter my-dummy-list (list 1 2 3))

(print "my dummy list before setf")
(print my-dummy-list)

(setf (second my-dummy-list) 7)
;; or like this
(setf (nth 1 my-dummy-list) 65)

(print "my dummy list after setf")
(print my-dummy-list)

(print "can also access list values using nth keyword")
(print (nth 0 my-dummy-list))


;; using higher order functions, it takes a function func, a list and performs some 
;; operation on a list
(print (mapcar #'string-upcase (list "Hello" "world!")))


(print (reduce #'(lambda (a b)
                     (* a b))
                 (list 10 20 30)))


(reduce #'(lambda (a b)
                     (format t "A: ~A, B: ~A~%" a b)
                     (* a b))
                 (list 1 2 3 4 5 6))


;; sorting

(print (sort (list 9 2 4 7 3 0 8) #'<))

;; defining classes and function classes

(defgeneric description (object)
  (:documentation "Return a description of an object."))


(defmethod description ((object integer))
  (format nil "The integer ~D" object))

(defmethod description ((object float))
  (format nil "The float ~3,3f" object))


(print (description 3.12))