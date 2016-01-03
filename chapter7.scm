(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda(l)
    (cond
     ((null? l) #t)
     ((symbol? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda(a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
	       (member? a (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote()))
     ((eq? a (car lat))
      (multirember a (cdr lat)))
     (else
      (cons (car lat) (multirember a (cdr lat)))))))


(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else
      (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) (quote()))
     ((set? lat) lat)
     ((member? (car lat) (cdr lat)) (cons (car lat) (makeset
						     (multirember (car
								   lat)
								  (cdr lat)))))
     (else
      (cons (car lat) (makeset (cdr lat)))))))


(define subset?
  (lambda (l1 l2)
    (cond
     ((null? l1) #t)
     ((member? (car l1) l2) (subset? (cdr l1) l2))
     (else #f))))

; subset? but using (and...) page 114
(define subset?
  (lambda (l1 l2)
    (cond
     ((null? l1) #t)
     (else
      (and (member? (car l1) l2) (subset? (cdr l1) l2))))))

; 114 book has a different solution
(define eqset?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)))
     ((member? (car l1) l2) (eqset? (cdr l1) (multirember (car l1)
							  l2)))
     (else
      #f))))

(define intersect?
  (lambda (l1 l2)
    (cond
     ((null? l1) #f)
     ((member? (car l1) l2) #t)
     (else
      (intersect? (cdr l1) l2)))))

; 115 intersect? using (or...)
(define intersect?
  (lambda (l1 l2)
    (cond
     ((null? l1) #f)
     (else
      (or (member? (car l1) l2) (intersect? (cdr l1) l2))))))

(define intersect
  (lambda (s1 s2)
    (cond
     ((null? s1) (quote()))
     ((member? (car s1) s2) (cons (car s1) (intersect (cdr s1) s2)))
     (else
      (intersect (cdr s1) s2)))))

(define union
  (lambda (s1 s2)
    (cond
     ((null? s1) s2)
     ((member? (car s1) s2) (union (cdr s1) s2))
     (else
      (cons (car s1) (union (cdr s1) s2))))))

(define member*
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     ((atom? (car lat))
      (cond
       ((eq? a (car lat)) #t)
       (else
	(member* a (cdr lat)))))
     (else
      (or (member* a (car lat)) (member* a (cdr lat)))))))


(define rember*
  (lambda (a lat)
    (cond
     ((null? lat) (quote()))
     ((atom? (car lat))
	     (cond
	      ((eq? a (car lat)) (rember* a (cdr lat)))
	      (else
	       (cons (car lat) (rember* a (cdr lat))))))
     (else
      (cons (rember* a (car lat)) (rember* a (cdr lat)))))))

(define intersectall
  (lambda (lset)
    (cond
     ((null? (cdr lset)) (car lset))
     (else
      (intersect (car lset) (intersectall (cdr lset)))))))
     
