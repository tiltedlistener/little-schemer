(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda (x)
    (+ x 1)))

(define sub1
  (lambda (x)
    (- x 1)))


(define adder
  (lambda (n m)
    (display n)
    (cond
     ((zero? m) n)
     (else (add1 (adder n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else 
      (+ (car tup) (addtup (cdr tup)))))))

(define mult
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else
      (+ n (mult n (sub1 m)))))))

(define tupPlus
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1) (null? tup2)) (quote ()))
     (else
      (cons (+ (car tup1) (car tup2)) (tupPlus (cdr tup1) (cdr tup2)))))))

(define pow
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else
      (* n (pow n (sub1 m)))))))

(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else
      (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
     ((> n (length lat)) (quote()))
     ((eq? 1 n)  (car lat))
     (else
      (pick (sub1 n) (cdr lat))))))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote()))
     ((number? (car lat)) (no-nums (cdr lat)))
     (else
      (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote()))
     ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
     (else
      (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else
      (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     (else
      (cond
       ((eq? a (car lat)) (+ 1 (occur a (cdr lat))))
       (else
	((occur a (cdr lat)))))))))

(define rempick
  (lambda (n lat)
    (cond
     ((null? lat) (quote()))
     ((< n 1) lat)
     ((= n 1) (cdr lat))
     (else
      (cons (car lat) (rempick (- n 1) (cdr lat)))))))
