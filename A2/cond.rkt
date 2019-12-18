;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (q1d-alt a? b? c?)
  (
   cond [(and(not a?)(not b?)(not c?)) 'larch]
        [(and a? b? c?) 'spruce]
        [a? 'hazel]
        [b? 'spruce]
        [c? 'hicory]
  ))