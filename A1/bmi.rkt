;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bmi) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (body-mass-index m h)
  (/ m (sqr h)))

(define (to-kg lb)
  (* 0.45359237 lb))

(define (to-m f i)
  (* 0.0254 (+ (* 12 f) i)))

(define (body-mass-index-imperial lb f i)
  (body-mass-index (to-kg lb) (to-m f i)))