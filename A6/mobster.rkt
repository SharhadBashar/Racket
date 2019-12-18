(define (make-goon name lon)
  (cons name lon))

(define goon-btb (make-goon "Boris, the Blade" (list 4 6 7)))
(define goon-btt (make-goon "Bullet Tooth Tony" (list 8 2 5)))
(define goon-ca (make-goon "Cousin Avi" (list 3 9 8)))
(define goon-fff (make-goon "Franky Four Fingers" (list 5 7 3)))


(define job-mule (list 5 0 1))
(define job-financer (list 3 8 5))
(define job-bribe (list 5 6 10))


(define (eval-goon Goon Job)
  (cond
    [(or (empty? Goon) (empty? Job)) 0]
    [(not (integer? (first Goon))) (eval-goon (rest Goon) Job)]
    [(< (first Goon) (first Job)) false]
    [else (+ (- (first Goon) (first Job)) (eval-goon (rest Goon) (rest Job)))]))