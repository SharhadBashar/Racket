;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname shortwalk) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Sharhad Bashar
;; CS 135 Fall 2019
;; Assignment 02, Problem 5a
;; ***************************************************
;;

(define (make-point  x y)
  (cons x (cons y empty)))

(define (x-coord Q)
  (first Q))

(define (y-coord Q)
  (second Q))

(define (make-step starting-point direction distance)
  (cond
    [(eq? direction 'N) (make-point (x-coord starting-point) (+ (y-coord starting-point) distance))]
    [(eq? direction 'E) (make-point (+ (x-coord starting-point) distance) (y-coord starting-point))]
    [(eq? direction 'S) (make-point (x-coord starting-point) (- (y-coord starting-point) distance))]
    [(eq? direction 'W) (make-point (- (x-coord starting-point) distance) (y-coord starting-point))]
    )
  )