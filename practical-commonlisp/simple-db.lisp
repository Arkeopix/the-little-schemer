;; A Simple database

;; filling CD's
(defvar *db* nil) ;global variable holding all the data

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd)
  (push cd *db*))

;; now we can (add-record (make-cd "Antipop" "Primus" 10 t))

;; looking at CD's

(defun dump-db ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*)) ; a consumes an element and prints it nice t is a tab, ~{~} is a loop

;; prompt the user
(defun prompt-read (prompt)
  (format t "~a: " prompt) 
  (force-output t) ; force-outputs is there so that format will not wait for \n to print
  (read-line *query-io*)) ; *query-io* is the input stream

(defun prompt-for-cds ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [Y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cds))
	 (if (not (y-or-n-p "Another? [Y/n]")) (return))))

;; Saving loading database
(defun save-db (filename)
  ;; with-open-file opens a file binds the stream to a variable, execute expressions and closes the file + clean close if error
  (with-open-file (out filename :direction :output :if-exists :supersede) ;open filename with options, binds stream to out
	(with-standard-io-syntax (print *db* out)))) ; with-standard-io-syntax makes sure that print behave nicely with special characters

(defun load-db (filename)
  (with-open-file (in filename)
	(with-standard-io-syntax (setf *db* (read in))))); setf specify a memory location and evaluates the second argument

;; querying the databse

(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) ; #' means go get the function named stuff, in this case a lambda
	   (equal (getf cd :artist) artist))
   *db*))
;; Ok. What if we want to write select by *
;; this is not very generic and we need to refactor the function

(defun select (select-fn)
  (remove-if-not select-fn *db*)) 

;; we call this func like this: (select #'func)
;; let's encapsulate this.

(defun select-artist (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

;; but we dont wnat to write a shitload of nearly identical functions
;; We need to generate predicate on the fly, and keyword parameters are here to help us

;; the following function returns a lambda
(defun where (&key title artist rating (ripped nil ripped-p)) ; ripped have a default value
  #'(lambda (cd)
	  (and
	   (if title (equal (getf cd :title) title) t)
	   (if artist (equal (getf cd :artist) artist) t)
	   (if rating (equal (getf cd :rating) rating) t)
	   (if ripped-p (equal (getf cd :ripped) ripped) t))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar ; mapcar calls its first argument on each element of its second arguments
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title    (setf (getf row :title) title))
               (if artist   (setf (getf row :artist) artist))
               (if rating   (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

;; remove duplication

(defun make-comparison-expr1 (field value)
  (list 'equal (list 'getf 'cd field) value))
;; OR
(defun make-comparison-expr2 (field value)
  `(equal (getf cd ,field) ,value)) ;; in a ' form a , force evaluation

(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr2 (pop fields) (pop fields))))

(defmacro where-macro (&rest clauses) ; rest turn the arguments in a list
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses)))) ; ,@ is like , but splices the value of the next expression
;; `(and ,(list 1 2 3))   ==> (AND (1 2 3))
;; `(and ,@(list 1 2 3))  ==> (AND 1 2 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

