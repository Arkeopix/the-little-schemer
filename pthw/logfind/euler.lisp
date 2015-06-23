;; I totaly suck a maths but i hear that project euler helps one gertting started
;; plus, a little maths wont hurt, i guess.

;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
;; The sum of these multiples is 23.
;; Find the sum of all the multiples of 3 or 5 below 1000.

;; helper function to generate a range, taken from SO
(defun make-range (end &key (start 0) (step 1))
  "generates a range of numbers from :start to end jumping step between each number"
  (loop for i from start below end by step collect i))

;; so at first i went with a copy of my errlang solution, using lists:foldl and lists:seq
;; unfortunately i was way over my head with this reduce function wich quite franckly, fucked up my brain.

;; (defun sum-it (x acc)
;;   (format t "x = ~D sum = ~D~%" x acc)
;;   (cond
;; 	((or (zerop (mod x 5)) (zerop (mod x 3)))
;; 	 ;(format t "x = ~D sum = ~D; x + sum = ~D~%" x sum (+ sum x))
;; 	 (+ acc x))
;; 	(t acc)))

;; (defun multiple-of-3-and-5 (limit)
;;   (format t "~{~D~%~}" (make-range limit))
;;   (reduce #'sum-it (make-range limit) :initial-value 0))

;; so juste went over http://www.unixuser.org/~euske/doc/cl/loop.html to see how i
;; could loop on numbers from 0 to n and found sum. hence the following
(defun sum-of-3-or-5 (max)
  (loop for i from 1 to max
	 if (or (zerop (mod i 3)) (zerop (mod i 5)))
	 sum i))

(defun problem1 ()
  (sum-of-3-or-5 999)) ; will produce 233168; great success

;; Each new term in the Fibonacci sequence is generated by adding the previous two terms.
;; By starting with 1 and 2, the first 10 terms will be:
;; 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
;; By considering the terms in the Fibonacci sequence whose values do not exceed four million,
;; find the sum of the even-valued terms.

;; helper function that produces the nth number in the fibonacci sequence
(defun fib (n)
  (cond
	((= 1 n) 1)
	((= 2 n) 2)
	(t (+ (fib (- n 1)) (fib (- n 2)))))) ; RPN FFS !! (+ 1 1) and not (1 + 1)

;; no major problem here
(defun sum-even-fib (a)
  ;; the let statement got me quite confused because x was not defined
  ;; but then i remebered that lisp standed for LIST Processing
  ;; so i moved the body of the function in the let statement and all was fine
  (let ((x (fib a))) 
	(cond
	  ((< x 4000000)
	   (cond
		 ((zerop (mod x 2))
		  (+ (fib a) (sum-even-fib (+ a 1))))
		 (t (sum-even-fib (+ a 1)))))
	  (t 0))))

(defun problem2 ()
  (sum-even-fib 1))

;; The prime factors of 13195 are 5, 7, 13 and 29.
;; What is the largest prime factor of the number 600851475143 ?

(defun get-largest-prime-factor (a lpf)
  (cond
	((> a lpf)
	 (cond
	   ((zerop (mod a lpf))
		(get-largest-prime-factor (/ a lpf) 2))
	   (t (get-largest-prime-factor a (+ lpf 1)))))
	(t lpf)))

;; went smouthly
(defun problem3 (a)
  (get-largest-prime-factor a 2))

;; A palindromic number reads the same both ways. The largest palindrome made
;; from the product of two 2-digit numbers is 9009 = 91 × 99.
;; Find the largest palindrome made from the product of two 3-digit numbers.

;; helper function to check for palindrom
(defun palindrom (s)
  (let ((a (write-to-string s :base 10)))
	(cond
	  ((string-equal (reverse a) a) t)
	  (t nil))))

(defun get-largest-palindrome-of-2-3-digit-number (d1 d2 max)
  (cond
	((= d1 99) max)
	((= d2 99)
	 (get-largest-palindrome-of-2-3-digit-number (- d1 1) 999 max))
	(t (let ((p (* d1 d2)))
		 (cond
		   ((and (> p max) (palindrom p))
			(get-largest-palindrome-of-2-3-digit-number d1 d2 p))
		   (t (get-largest-palindrome-of-2-3-digit-number d1 (- d2 1) max)))))))
  
(defun problem4 ()
  (get-largest-palindrome-of-2-3-digit-number 999 999 0))

;; 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
;; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

;; i'm glad i checked, common lisp have a builtin function for gcd/lcm
;; (defun gcd (a b)
;;   (loop for tmp from a below)))

;; i loled a little at this solution
;; (defun problem5 ()
;;   (lcm 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))

(defun get-lcm (a b)
  (cond ((or (= a 0) (= b 0)) 0)
		(t (abs (/ (* a b) (gcd a b))))))

(defun my-lcm (&rest list)
  (reduce #'get-lcm list :initial-value 1)) ;; hooray =)

(defun problem5 (&rest list)
  (my-lcm list))

;; The sum of the squares of the first ten natural numbers is,
;; 		12 + 22 + ... + 102 = 385
;; The square of the sum of the first ten natural numbers is,
;; 		(1 + 2 + ... + 10)2 = 552 = 3025
;; Hence the difference between the sum of the squares of the first ten natural
;; numbers and the square of the sum is 3025 − 385 = 2640.
;; Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

(defun sum-square (max)
  (loop for i from 1 to max sum (expt i 2))) ; do not forget loop, it's a powerfull structure

(defun square-sum (max)
  (loop for i from 1 to max sum i))

(defun sum-square-difference (max)
  (- (expt (square-sum max) 2) (sum-square max)))

(defun problem6 ()
  (sum-square-difference 100))


;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
;; What is the 10001st prime number?
