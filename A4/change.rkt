;; The function make-change produces the list of change in coins
;; consuming the total sum of the values of the coins
;; Nat -> listof Sym

(define toonie-price 200)
(define loonie-price 100)
(define quarter-price 25)
(define dime-price 10)
(define nickel-price 5)

(define (make-change total)
  (cond [(= (remainder total 5) 1) (make-change (- total 1))]
        [(= (remainder total 5) 2) (make-change (- total 2))]
        [(= (remainder total 5) 3) (make-change (+ total 2))]
        [(= (remainder total 5) 4) (make-change (- total 1))]
        [else (cond [(<= total 0) empty]
                    [(>= total toonie-price)
                     (cons 'toonie (make-change (- total toonie-price)))]
                    [(and (>= total loonie-price) (< total toonie-price))
                     (cons 'loonie (make-change (- total loonie-price)))]
                    [(and (>= total quarter-price) (< total loonie-price))
                     (cons 'quarter (make-change (- total quarter-price)))]
                    [(and (>= total dime-price) (< total quarter-price))
                     (cons 'quarter (make-change (- total dime-price)))]
                    [(< total dime-price)
                     (cons 'nickel (make-change (- total nickel-price)))])]))
;; Tests:
(check-expect (make-change 41)
              (cons 'quarter (cons 'quarter (cons 'nickel empty))))
(check-expect (make-change 102)
              (cons 'loonie empty))

















