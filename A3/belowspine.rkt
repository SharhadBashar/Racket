;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname belowspine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Sharhad Bashar
;; CS 135 Fall 2019
;; Assignment 02, Problem 4b
;; ***************************************************
;;

(define (x-coord Q) (first Q))

(define (y-coord Q) (second Q))

(define (slope y2 y1 x2 x1)
  (/ (- y2 y1) (- x1 x1)))

(define (below-spine? P1 P2 ab)
  
  (
   cond
    [(<= (x-coord ab) (x-coord P1)) (cond
                                      [(>= (slope (y-coord P1) 0 (x-coord P1) 0) (slope (y-coord ab) 0 (x-coord ab) 0)) true]
                                      [else false])]
    [(<= (x-coord ab) (x-coord P2)) (cond
                                      [(<= (y-coord ab) (+(*(slope (x-coord P1)(x-coord P2) (y-coord P1) (y-coord P2))(- (x-coord ab) (x-coord P1))) (y-coord P1))) true]
                                      [else false])]
    [(<= (x-coord ab) 10) (cond
                            [(>= (slope (y-coord P2) 0 10 (x-coord P2)) (slope (y-coord ab) 0 10 (x-coord ab))) true]
                            [else false])]
    [else false]
  )
)