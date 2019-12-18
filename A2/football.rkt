;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname football) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (intentional-grounding-correct? imminent-pressure inside-pocket team-member eligible-receiver)
  (not(
   and imminent-pressure inside-pocket team-member eligible-receiver
  ))
)