(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

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
