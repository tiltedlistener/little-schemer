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

