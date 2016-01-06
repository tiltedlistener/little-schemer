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

;; These are incorrect. Sorry but lazy copying happened. 
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


;; Starting page 132

; Original Methods
(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     ((eq? (car lat) old)
      (cons new lat))
      (else
       (cons (car lat) (insertl new old (cdr lat)))))))

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     ((eq? (car lat) old)
      (cons (car lat) (cons new (cdr lat))))
      (else
       (cons (car lat) (insertR new old (cdr lat)))))))

; Sequences
(define seqR
  (lambda (new old l)
     (cons old (cons new (cdr l)))))

(define seqL
  (lambda (new old l)
    (cons new l)))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) (quote()))
       ((eq? old (car l)) (seq new old l))
       (else
	(cons (car l) ((insert-g seq) new old (cdr l))))))))

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))

; Subst
(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     ((eq? (car lat) old)
      (cons new (cdr lat)))
      (else
       (cons (car lat) (subst new old (cdr lat)))))))

(define seqS
  (lambda (new old l)
    (cons new (cdr l))))

(define subst (insert-g seqS))

(define seqrem
  (lambda (new old l)
    l))
