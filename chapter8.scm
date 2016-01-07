(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


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

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote()))
     ((eq? a (car lat)) (multirember a (cdr lat)))
     (else
      (cons (car lat) (multirember a (cdr lat)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) (quote()))
       ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
       (else
	(cons (car lat) ((multirember-f test?) a (cdr lat))))))))

(define multirember-eq (multirember-f eq?))

(define eq-tuna?
  (lambda (a)
    (eq? 'tuna a)))

(define multiremberT
  (lambda (test?)
    (lambda (lat)
      (cond
       ((null? lat) (quote()))
       ((test? (car lat)) ((multiremberT test?) (cdr lat)))
       (else
	(cons (car lat) ((multiremberT test?) (cdr lat))))))))


(define multirember-tuna? (multiremberT eq-tuna?))

(define multirember&co
  (lambda (a lat col)
    (cond
    ((null? lat) (col '() '()))
    ((eq? (car lat) a)
     (multirember&co a (cdr lat)
		     (lambda (newlat seen)
		       (col newlat (cons (car lat) seen)))))
    (else
     (multirember&co a (cdr lat)
		     (lambda (newlat seen)
		       (col (cons (car lat) newlat) seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

