;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname winsys) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))

;; An RTree (Rectangle Tree) is one of:
;; * Sym (a leaf)
;; * XNode (a boundary node for an x value)
;; * YNode (a boundary node for a y value)

(define-struct xnode (val left right))
;; An XNode is a (make-xnode Nat RTree RTree)
;; requires: val > every xnode-val in left RTree
;;           val < every xnode-val in right RTree

(define-struct ynode (val below above))
;; A YNode is a (make-ynode Nat RTree RTree)
;; requires: val > every ynode-val in below RTree
;;           val < every ynode-val in above RTree

;; Q3a
(define (rtree-template tree)
  (cond [(symbol? tree) ...]
        [(xnode? tree) (... (xnode-val tree) ...
                            (xnode-left tree) ...
                            (xnode-right tree) ...)]
        [(ynode? tree) (... (ynode-val tree) ...
                            (ynode-below tree) ...
                            (ynode-above tree) ...)]))

(define (xnode-template tree)
  (cond [(empty? tree) ...]
        [(xnode? tree) (... (xnode-val tree) ...
                            (xnode-left tree) ...
                            (xnode-right tree) ...)]))

(define (ynode-template tree)
  (cond [(empty? tree) ...]
        [(ynode? tree) (... (ynode-val tree) ...
                            (ynode-below tree) ...
                            (ynode-above tree) ...)]))





;; Q3b

;; Test cases

;; testcase 1: a square
(define l1 (make-xnode  0  'None 'Content))
(define r1 (make-xnode 70   l1   'None))
(define b1 (make-ynode  0  'None  r1))
(define t1 (make-ynode 70   b1   'None))

;; testcase 2: a simple window
(define x60 (make-xnode 60 'Menu   'x))
(define y60 (make-ynode 60 'Content x60))
(define  l2 (make-xnode  0 'None    y60))
(define  r2 (make-xnode 70  l2     'None))
(define  b2 (make-ynode  0 'None    r2))
(define  t2 (make-ynode 70  b2     'None))

;; produces the symbol from the RTree that is associated
;; with the rectangle that contains pos
;; rt-lookup: RTree Posn -> Sym
;; Examples:

;; pos on bottom boundary
(check-expect (rt-lookup t1 (make-posn 0 35)) 'Content)

;; pos on left boundary
(check-expect (rt-lookup t1 (make-posn 35 0)) 'Content)

(define (rt-lookup rt pos)
  (cond [(symbol? rt) rt]
        [(xnode? rt) (cond [(>= (posn-x pos) (xnode-val rt))
                            (rt-lookup (xnode-right rt) pos)]
                           [else (rt-lookup (xnode-left rt) pos)])]
        [(ynode? rt) (cond [(>= (posn-y pos) (ynode-val rt))
                            (rt-lookup (ynode-above rt) pos)]
                           [else (rt-lookup (ynode-below rt) pos)])]
        [else 'none]))


;; pos on top boundary
;; recall that rectangle only includes x < (xnode-val rt)
(check-expect (rt-lookup t1 (make-posn 70 35)) 'None)

;; pos on right boundary
;; recall that rectangle only includes y < (ynode-val rt)
(check-expect (rt-lookup t1 (make-posn 35 70)) 'None)