;; ** Decreasing List **
;; The function DECREASINGlist produces a decreasing list of natural numbers
;; from a natural number and strictly stops at 1
;; requires natural number >=2
;; Nat -> listof Nat

(define (DECREASINGlist Num)   ;Num must be atleast 2
  (cond [(= Num 1) empty]
        [else (cons Num (DECREASINGlist (- Num 1)))]))

;; The function divisibility-check produces whether a Natural number
;; is not perfectly divisible by the elements in a list of numbers (listofNum)
;; Not perfectly divisible produces true
;; Nat (listof Nat) -> Bool

(define (divisibility-check Num listofNum)
  (cond
    [(empty? listofNum) true]
    [(= (remainder  Num (first listofNum)) 0) false]
    [else (divisibility-check Num (rest listofNum))]))
;; Tests:
(check-expect (divisibility-check 23 (DECREASINGlist 22)) true)
(check-expect (divisibility-check 47 (DECREASINGlist 46)) true)

;; The function prime? produces whether a number is prime or not
;; consuming a natural number
;; Nat -> Bool

(define (prime? number)
  (cond [(or (= number 0) (= number 1)) false]
        [(divisibility-check number (DECREASINGlist (sub1 number))) true]
        [else false]))

;; The function break-list produces the elements in a list for a given range
;; consuming the upper bound and the lower bound
;; Nat Nat -> listof Num
;; Examples:
(check-expect (break-list 3 4) (cons 3 (cons 4 empty)))
(check-expect (break-list 40 40) (cons 40 empty))

(define (break-list lower-bound upper-bound)
  (cond [(= upper-bound lower-bound) (cons upper-bound empty)]
        [else (cons lower-bound (break-list (add1 lower-bound) upper-bound))]))
;;Tests:
(check-expect (break-list 0 2) (cons 0 (cons 1 (cons 2 empty))))
(check-expect (break-list 0 0) (cons 0 empty))

;; The function prime-range prouces the list of prime numbers in a given range
;; consuming the limits L1 and L2, where L1 and L2 are natural numbers
;; Nat Nat -> listof Nat

  (define (prime-range L1 L2)
  (cond
     [(> L1 L2) empty]
     [(= L1 L2) (cond [(prime? L1) (cons L2 empty)]
                      [else empty])]
     [(prime? (first (break-list L1 L2))) (cons L1 (prime-range (+ 1 L1) L2))]
     [else (prime-range (add1 L1) L2)]))
;;Tests:
(check-expect (prime-range 2 7) (cons 2 (cons 3 (cons 5 (cons 7 empty)))))
(check-expect (prime-range 2 3) (cons 2 (cons 3 empty)))










