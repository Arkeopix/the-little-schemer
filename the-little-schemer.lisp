(defun atom? (x)
  (not (listp x)))

(defun add1 (n)
  (+ n 1))

(defun sub1 (n)
  (- n 1))

;; implement lat: are all elements or S-expression
;; (nice explanation on accepted answer http://stackoverflow.com/questions/10771107/lisp-list-vs-s-expression)
;; of a list atoms ?
(defun lat? (list)
  (cond
	((null list) t) ; verify if the list is nil, if yes -> t (true)
	((atom? (car list)) (lat? (cdr list))) ; verify if first S-expression is an atom, if yes -> recurse on lat with rest of the list
	(t nil))) ; else nil

;; implement member?
;; is atom a member of lat ?
(defun member? (atom lat)
  (cond
	((null lat) nil) ; Is the list empty ? -> no atom match
	(t (or (eq (car lat) atom) ; is the first element of the list lat equal to the atom atom ?
		   (member? atom (cdr lat)))))) ; recurse

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The first rule when recursing is:                                ;;
;; Always ask null as the first question in expressing any function ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rember (atom lat)
  (cond
	((null lat) '())
	(t (cond
		 ((eq (car lat) atom) (cdr lat))
		 (t (rember atom (cdr lat)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The second rule is:                                              ;;
;; Use cons to build list                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rember and rember-cons remove the member atom from the list lat
(defun rember-cons (atom lat)
  (cond
	((null lat) '()) ; is the list null ?
	((eq (car lat) atom) (cdr lat)) ; is the atom eq to the list element ? if yes return crd of lat (which may consed)
	(t (cons (car lat) ; build a list whose values will come from
			 (rember-cons atom (cdr lat)))))) ; the succesive recursive calls to rember-cons

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the third rull is:                                               ;;
;; When building a list, describe the first typical element, and    ;;
;; then cons it onto the natural recursion                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; firsts takes a list lists and construct a list with each first element of each list
(defun firsts (list)
  (cond
	((null list) '()) ; first rule, is the list null ?
	;; we (car (car list)) because we need the first value of the first list
	;; we cons the resulting value onto the recursion of first (cdr list)
	(t (cons (car (car list)) (firsts (cdr list)))))) ; second and third rule
;;           |   typical    | |     natural     |
;;           |   element    | |    recursion    |

;; insertR/L takes 3 arguments (new old lat) and creates a new list with new inserted
;; to the right/left of the first occurence of old
(defun insertR (new old lat)
  (cond
	((null lat) '())
	(t (cond
		 ((eq (car lat) old) ;if first atom is equal to old
		  (cons old (cons new (cdr lat)))) ; return cons of new and cdr of lat
		 (t (cons (car lat) (insertR new old (cdr lat))))))))

(defun insertL (new old lat)
  (cond
	((null lat) '())
	(t (cond
		 ((eq (car lat) old)
		  (cons new (cons old (cdr lat))))
		 (t (cons (car lat) (insertL new old (cdr lat))))))))

;; subst replace old1 and/or old2 with new in lat
(defun subst2 (new old1 old2 lat)
  (cond
	((null lat) '())
	(t (cond
		 ((or (eq (car lat) old1) (eq (car lat) old2))
		  (cons new (cdr lat)))
		 (t (cons (car lat) (subst2 new old1 old2 (cdr lat))))))))

;; multirember removes all occurence of atom atom in list lat
(defun multirember (atom lat)
  (cond
	((null lat) '())
	((eq (car lat) atom) ; if atom match car lat
	 (multirember atom (cdr lat))) ; recurse with the cdr of the list so that we are sure to remove all occurence
	(t (cons (car lat) (multirember atom (cdr lat)))))) ; else we just cons the car of lat (typical case) onto the natural recursion


;; multiinsertR
(defun multiinsertR (new old lat)
  (cond
	((null lat) '())
	(t (cond
		 ((eq (car lat) old)
		  (cons old (cons new (multiinsertR new old (cdr lat)))))
		 (t (cons (car lat) (multiinsertR new old (cdr lat))))))))

(defun multiinsertL (new old lat)
  (cond
	((null lat) '())
	(t (cond
		 ((eq (car lat) old)
		  (cons new (cons old (multiinsertL new old (cdr lat)))))
		 (t (cons (car lat) (multiinsertL new old (cdr lat))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The fourth rule is:                                             ;;
;; Always change at least one argument while recurring. It         ;;
;; must be changed to be closer to termination. The changing       ;;
;; argument must be tested in the termination condition:           ;;
;; when using cdr, test termination with null?.                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; multisubst changes all occurences of atom old with atom new in list lat
(defun multisubst (new old lat)
  (cond
	((null lat) '())
	(t (cond
		 ((eq (car lat) old)
		  (cons new (multisubst new old (cdr lat))))
		 (t (cons (car lat) (multisubst new old (cdr lat))))))))

;; write the function + with zerop add1 and sub1
(defun o+ (a b)
  (cond
	((zerop b) a) ; first rule bitch
	(t (add1 (o+ a (sub1 b))))))

;; write the - function with sub1
(defun o- (a b)
  (cond
	((zerop b) a)
	(t (sub1 (o- a (sub1 b))))))

;; so what we did here is that we reduced the second argument to zero
;; we then substract 1 from the result as many time as it needed to go down to 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the first rule updated is:                                   ;;
;; When recurring on a list of atoms, lat, ask two questions    ;;
;; about it: ( null ? lat) and else.                            ;;
;; When recurring on a number, n , ask two questions about      ;;
;; it: (zero ? n) and else.                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun addtup (tup)
  (cond
	((null tup) 0)
	(t (+ (car tup) (addtup (cdr tup)))))) ; we use + as cons -> rule 2 bitch

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the fourth rule updated is:                                  ;;
;; Always change at least one argument while recurring. It      ;;
;; must be changed to be closer to termination. The changing    ;;
;; argument must be tested in the termination condition:        ;;
;; when using cdr, test termination with null? and              ;;
;; when using sub1 , test termination with zero ?.              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; write the * function
(defun o* (a b)
  (cond
	((zerop b) a)
	(t (+ a (* a (sub1 b))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the fifth rule is:                                               ;;
;; When building a value with + , always use 0 for the value of the ;;
;; terminating line, for adding 0 does not change the value of an   ;;
;; addition.                                                        ;;
;; When building a value with x , always use 1 for the value of the ;;
;; terminating line, for multiplying by 1 does not change the value ;;
;; of a multiplication.                                             ;;
;; When building a value with                                       ;;
;; of the terminating line.                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; write the function tup+ which add each element of tow tuples betwwen them
(defun tup+ (t1 t2)
  (cond
	((null t1) t2)
	((null t2) t1)
	(t (cons (+ (car t1) (car t2)) (tup+ (cdr t1) (cdr t2))))))

;; write the function >
(defun my> (a b)
  (cond
	((zerop a) nil)
	((zerop b) t)
	(t (my> (sub1 a) (sub1 b)))))

;; write the function <
(defun my< (a b)
  (cond
	((zerop b) nil)
	((zerop a) t)
	(t (my< (sub1 a) (sub1 b)))))

;; write the function = using < and >
(defun my= (a b)
  (cond
	((my> a b) nil)
	((my< a b) nil)
	(t t)))


;; write the expt function
(defun myexpt (a b)
  (cond
	((zerop b) 1)
	(t (* a (myexpt a (sub1 b))))))

;; write the / function
(defun mydiv (a b)
  (cond
	((my< a b) 0)
	(t (add1 (mydiv (- a b) b)))))

;; so in the end, here is the decomposition
;; (/ 15 4) = 1 + (/ 11 4)
;;          = 1 + (1 + (/ 7 4))
;;          = 1 + (1 + (1 + (/ 3 4)))
;;          = 1 + (1 + (1 + 0))


;; write the function len
(defun mylen (list)
  (cond
	((null list) 0)
	(t (add1 (mylen (cdr list))))))

;; write the function pick
(defun mypick (n list)
  (cond
	((zerop (sub1 n)) (car list))
	(t (mypick (sub1 n) (cdr list)))))

;; write the rempick function
(defun rempick (n list)
  (cond
	((zerop n) (cdr list))
	(t (cons (car list) (rempick (sub1 n) (cdr list))))))

;; write a function no-nums
(defun no-nums (lat)
  (cond
	((null lat) '())
	(t (cond
		 ((numberp (car lat))
		  (no-nums (car lat)))
		 (t (cons (car lat) (no-nums (cdr lat))))))))

;; write the all-nums function
(defun all-nums (lat)
  (cond
	((null lat) '())
	(t (cond
		 ((numberp (car lat))
		  (cons (car lat) (all-nums (cdr lat))))
		 (t (all-nums (cdr lat)))))))

;; write the function epan
(defun epan (a1 a2)
  (cond
	((and (numberp a1) (numberp a2))
	 (= a1 a2))
	((or (numberp a1) (numberp a2))
	 nil)
	(t (eq a1 a2))))

;; write the occur function
(defun occur (atom lat)
  (cond
	((null lat) 0)
	(t (cond
		 ((eq (car lat) atom)
		  (add1 (occur atom (cdr lat))))
		 (t (occur atom (cdr lat)))))))

;; write the one function
(defun onep (n)
  (cond
	((numberp n)
	 (= 1 n))
	(t nil)))

;; or
(defun one (n) (= 1 n))
;; but this sucks because it will crash if n is not a number

;; rewrite rempick with onep
(defun rempick2 (n list)
  (cond
	((onep n) (cdr list))
	(t (cons (car list) (rempick2 (sub1 n) (cdr list))))))

;; write the rember* function
(defun rember* (atom list)
  (cond
	((null list) '())
	((atom? (car list)) ; if it's an atom do your stuff
	 (cond
	   ((eq atom (car list))
		(rember* atom (cdr list)))
	   (t (cons (car list) (rember* atom (cdr list))))))
	(t (cons (rember* atom (car list)) ; else recur on the the newfound list
			 (rember* atom (cdr list))))))

;; write the insertR function
(defun insertR* (new old list)
  (cond
	((null list)'())
	((atom? (car list))
	 (cond
	   ((eq (car list) old)
		(cons old (cons new (insertR* new old (cdr list)))))
	   (t (cons (car list) (insertR* new old (cdr list))))))
	(t (cons (insertR* new old (car list)) (insertR* new old (cdr list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The first rule final is: 
;; When recurring on a list of atoms, lat , ask two questions            ;;
;; about it: ( null? lat) and else.                                      ;;
;; When recurring on a number, n , ask two questions about               ;;
;; it: (zero ? n) and else.                                              ;;
;; When recurring on a list of S-expressions, l, ask three               ;;
;; question about it: ( null? l ) , ( atom ? ( car l) ) , and else.      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; write the occur* function
(defun occur* (atom list)
  (cond
	((null list) 0)
	((atom? (car list))
	 (cond
	   ((eq (car list) atom)
		(add1 (occur* atom (cdr list))))
	   (t (occur* atom (cdr list)))))
	(t (+ (occur* atom (car list)) (occur* atom (cdr list))))))

;; write the subst* function
(defun subst* (new old list)
  (cond
	((null list) '())
	((atom? (car list))
	 (cond
	   ((eq (car list) old)
		(cons new (subst* new old (cdr list))))
	   (t (cons (car list) (subst* new old (cdr list))))))
	(t (cons (subst* new old (car list)) (subst* new old (cdr list))))))

;; write the insertL* function
(defun insertL* (new old list)
  (cond
	((null list) '())
	((atom? (car list))
	 (cond
	   ((eq (car list) old)
		(cons new (cons old (insertL* new old (cdr list)))))
	   (t (cons (car list) (insertL* new old (cdr list))))))
	(t (cons (insertL* new old (car list)) (insertL* new old (cdr list))))))

;; write the member* function
(defun member* (atom list)
  (cond
	((null list) nil)
	((atom? (car list))
	 (cond
	   ((eq (car list) atom)
		t)
	   (t (member* atom (cdr list)))))
	(t (member* atom (car list)))))
;;sans les mains wesh

;; write the leftmost function
(defun leftmost (list)
  (cond
	((atom? (car list)) (car list))
	(t (leftmost (car list)))))

;; write the eqlist function
(defun eqlist (l1 l2)
  (cond
	((and (null l1) (null l2)) t)
	((and (null l1) (atom? (car l2))) nil)
	((null l1) nil)
	((and (atom? (car l1)) (atom? (car l2)))
	 (and (epan (car l1) (car l2))
		  (eqlist (cdr l1) (cdr l2))))
	((atom? (car l1)) nil)
	((null l2) nil)
	((atom? (car l2)) nil)
	(t (and (eqlist (car l1) (car l2))
			(eqlist (cdr l1) (cdr l2))))))

;; rewcrite the eqlist function
(defun eqlist2 (l1 l2)
  (cond
	((and (null l1) (null l2)) t)
	((or (null l1) (null l2)) nil)
	((and (atom? (car l1))
		  (atom? (car l2)))
	 (and (epan (car l1) (car l2))
		  (eqlist2 (cdr l1) (cdr l2))))
	((or (atom? (car l1))
		 (atom? (car l2)))
	 nil)
	(t (and (eqlist2 (car l1) (car l2))
			(eqlist2 (cdr l1) (cdr l2))))))

;; write the equal function
(defun myequal (se1 se2)
  (cond
	((and (atom? se1) (atom? se2))
	 (epan se1 se2))
	((or (atom? se1) (atom? se2)) nil)
	(t (eqlist2 se1 se2))))

;; re rewcrite eqlist using equal
(defun eqlist3 (l1 l2)
  (cond
	((and (null l1) (null l2)) t)
	((or (null l1) (null l2)) nil)
	(t (and (myequal (car l1) (car l2))
			(eqlist3 (cdr l1) (cdr l2))))))

;; rewrite rember to take a list insetad of a lat and atom by s-expression
(defun rember-lat (s list)
  (cond
	((null list) '())
	((myequal (car list) s) (cdr list))
	(t (cons (car list) (rember-lat s (cdr list))))))

;; write the numbered function
(defun numbered (aexp)
  (cond
	((atom? aexp) (numberp aexp))
	(t (and (numbered (car aexp))
			(numbered (car (cdr (cdr aexp))))))))

;; write the value function
(defun value (exp)
  (cond
	((atom? exp) exp)
	((eq  (car (cdr exp)) '+) ;(3 + 4) -> +
	 (+ (value (car exp)) (value (car (cdr (cdr exp)))))) ; (3 + 4) -> (+ 3 4)
	((eq (car (cdr exp)) '*)
	 (* (value (car exp)) (value (car (cdr (cdr exp))))))
	(t (myexpt (value (car exp)) (value (car (cdr (cdr exp))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Seventh Commandment                                           ;;
;; Recur on the subparts that are of the same nature:                ;;
;; • On the sublists of a list.                                      ;;
;; • On the subexpressions of an arithmetic expression.              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; write a function to extract the first sub expression
(defun firstsubexpr (exp)
  (car (cdr exp)))

;; write a function to extract the second sub expresion
(defun secondsubexpr (exp)
  (car (cdr (cdr exp))))

;; replace the car with an operator function
(defun operator (exp)
  (car exp))

;; rewrite value with those function
(defun value2 (exp)
  (cond
	((atom? exp) exp)
	((eq (operator exp) '+)
	 (+ (value (firstsubexpr exp)) (value (secondsubexpr exp))))
	((eq (car (cdr exp)) '*)
	 (* (value (firstsubexpr exp)) (value (secondsubexpr exp))))
	(t (myexpt (firstsubexpr exp) (value (secondsubexpr exp))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Eighth Commandment                                       ;;
;; Use help functions to abstract from representations.         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; now we change representation. instead of of arabic symbols let's use
;; emtptylist as numbers. ie () is 0, (()) is 1, (()()) is 2 and so on.

;; write a function to test for 0
(defun pero (n)
  (null n))

;; write a function like add1
(defun padd1 (n)
  (cons '() n))

;; sub1
(defun psub1 (n)
  (cdr n))

;; rewrite + using this representation
(defun p+ (a b)
  (cond
	((pero b) a)
	(t (padd1 (p+ a (psub1 b))))))

;; Write the set function
(defun myset (lat)
  (cond
	((null lat) t)
	((member? (car lat) (cdr lat)) nil)
	(t (myset (cdr lat)))))

;; write the makeset function
(defun makeset (lat)
  (cond
	((null lat) '())
	((member? (car lat) (cdr lat))
	 (makeset (cdr lat)))
	(t (cons (car lat) (makeset (cdr lat))))))

(defun makeset2 (lat)
  (cond
	((null lat) '())
	(t (cons (car lat) (multirember (car lat) (cdr lat))))))

;; write the subset function
(defun subset? (set1 set2)
  (cond
	((null set1) t)
	((member? (car set1) set2)
	 (subset? (cdr set1) set2))
	(t nil)))
