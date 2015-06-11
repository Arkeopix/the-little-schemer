;; I totaly suck a maths but i hear that project euler helps one gertting started
;; plus, a little maths wont hurt, i guess.

;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
;; The sum of these multiples is 23.
;; Find the sum of all the multiples of 3 or 5 below 1000.

(defun make-range (end &key (start 0) (step 1))
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
  (sum-of-3-or-5 999)) ; will produce 233168
