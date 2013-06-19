(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define rember
  (lambda (a lat)
    (cond 
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (rember a
                            (cdr lat))))))))
