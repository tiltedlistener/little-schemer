; page 149

(define pick
  (lambda (pos lat)
    (cond
     ((eq? 0 (- pos 1)) (car lat))
     (else
      (pick (- pos 1) (cdr lat))))))


(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a init lat)
    (cond
     ((eq? init a) #t)
     ((not (integer? init)) #f)
     (else
      (keep-looking a (pick init lat) lat)))))

					; For page 153

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

(define shift
  (lambda (pair)
    (build (first (first pair))
	   (build (second (first pair)) (second pair)))))

(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora)) (align (shift pora)))
     (else
      (build (first pora) (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (+ (length* (first pora)) (length* (second pora)))))))
     
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))
