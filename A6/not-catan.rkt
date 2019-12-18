(define (isNat? lon)
  (cond
    [(empty? lon) false]
    [(and (integer? (first lon)) (> (first lon) 0)) (isNat? (rest lon))]
    [else true]))

(define (make-inventory blocks wood sheep wheat rocks)
  (cons blocks (cons wood (cons sheep (cons wheat (cons rocks empty))))))


(define (valid-inventory? inventory)
  (cond
    [(isNat? inventory) false]
    [(> (length inventory) 5) false]
    [(< (length inventory) 5) false]
    [else (and (>= (third(rest inventory)) 4) (>= (third(rest inventory)) (third inventory)))]))