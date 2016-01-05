(define rember?
  (lambda (a lat)
    (cond
     ((null? lat) (quote()))
      (else (cond
	     ((eq? (car lat) a) (cdr lat))
	     (else (cons (car lat)
			 (rember? a
				 (cdr lat)))))))))


(define rember-f
  (lambda (test? a lat)
    (cond
     ((null? lat) (quote()))
      (else (cond
	     ((test? (car lat) a) (cdr lat))
	     (else (cons (car lat)
			 (rember-f test? a
				   (cdr lat)))))))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) (quote()))
       ((test? a (car l)) (cdr l))
       (else
	(cons (car l) (rember-f test? a (cdr l))))))))

(define rember-eq? (rember-f eq?))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     ((eq? (car lat) old)
      (cons new lat))
      (else
       (cons (car lat) (insertl new old (cdr lat)))))))

(define insertL-f
  (lambda (test? new old lat)
    (cond
     ((null? lat) (quote()))
     ((test? (car lat) old) (cons new lat))
      (else
       (cons (car lat) (insertl-f test? new old (cdr lat)))))))

(define insertL-f
  (lambda (test?)
   (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     ((test? (car lat) old)
      (cons new lat))
      (else
       (cons (car lat) (insertl-f test? new old (cdr lat))))))))

(define insertL-eq? (insertL-f eq?))

