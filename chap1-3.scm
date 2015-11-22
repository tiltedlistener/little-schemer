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

(define rember?
  (lambda (a lat)
    (cond
     ((null? lat) (quote()))
      (else (cond
	     ((eq? (car lat) a) (cdr lat))
	     (else (cons (car lat)
			 (rember? a
				 (cdr lat)))))))))

(define firsts
  (lambda (l)
	 (cond
	  ((null? l) (quote()))
	  (else (cons (car (car l))
		      (firsts (cdr l)))))))


(define insertR
  (lambda (new old lat)
    (display lat)
    (cond
     ((null? lat) (quote()))
     ((eq? (car lat) old)
      (cons (car lat) (cons new (cdr lat))))
      (else
       (cons (car lat) (insertR new old (cdr lat))))))))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     ((eq? (car lat) old)
      (cons new lat))
      (else
       (cons (car lat) (insertl new old (cdr lat)))))))


(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     ((eq? (car lat) old)
      (cons new (cdr lat)))
      (else
       (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) (quote()))
     ((eq? (car lat) o1)
      (cons new (cdr lat)))
     ((eq? (car lat) o2)
      (cons new (cdr lat)))
      (else
       (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote()))
     ((eq? a (car lat))
      (multirember a (cdr lat)))
     (else
      (cons (car lat) (multirember a (cdr lat)))))))


(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     ((eq? (car lat) old)
      (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
      (else
       (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     ((eq? (car lat) old)
      (cons new (cons (car lat) (multiinsertL new old (cdr lat)))))
      (else
       (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     ((eq? (car lat) old)
      (cons new (multisubst new old (cdr lat))))
      (else
       (cons (car lat) (multisubst new old (cdr lat)))))))
