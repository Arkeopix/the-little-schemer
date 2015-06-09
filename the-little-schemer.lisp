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
