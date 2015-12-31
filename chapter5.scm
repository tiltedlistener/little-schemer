(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


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

(define insertR*
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     ((atom? (car lat))
      (cond
       ((eq? (car lat) old) (cons (car lat) (cons new (insertR* new
								old (cdr lat)))))
       (else
	(cons (car lat) (insertR* new old (cdr lat))))))
     (else
      (cons (insertR* new old (car lat)) (insertR* new old (cdr lat)))))))


(define occur*
  (lambda (a lat)
    (display lat)
    (cond
     ((null? lat) 0)
     ((atom? (car lat))
      (cond
       ((eq? a (car lat)) (+ 1 (occur* a (cdr lat))))
       (else
	(occur* a (cdr lat)))))
     (else
      ((+ (occur* a (car lat)) (occur* a (cdr lat))))))))

(define subst*
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     ((atom? (car lat))
      (cond
       ((eq? old (car lat)) (cons new (subst* new old (cdr lat))))
       (else
	(cons (car lat) (subst* new old (cdr lat))))))
     (else
      (cons (subst* new old (car lat)) (subst* new old (cdr lat)))))))

(define insertL*
  (lambda (new old lat)
  (cond
   ((null? lat) (quote()))
   ((atom? (car lat))
    (cond
     ((eq? old (car lat)) (cons new (cons (car lat) (insertL* new old
							      (cdr
							       lat)))))
     (else
      (cons (car lat) (insertL* new old (cdr lat))))))
   (else
    (cons (insertL* new old (car lat)) (insertL* new old (cdr
							  lat)))))))

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

(define leftmost
  (lambda (lat)
    (cond
     ((atom? (car lat)) (car lat))
     (else
      (leftmost (car lat))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((or (null? l1) (null? l2)) (and (null? l1) (null? l2)))
     ((or (atom? (car l1)) (atom? (car l2)))
      (and (and (eq? (car l1) (car l2))) (eqlist? (cdr l1) (cdr
								  l2))))
     (else
      (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr
							  l2)))))))


     
