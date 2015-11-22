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
