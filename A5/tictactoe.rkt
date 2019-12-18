;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tictactoe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define grid1 '((_ _ _)
                (_ _ _)
                (_ _ _)))

(define grid2 '((X O O O _)
                (X X O _ _)
                (_ _ _ _ _)
                (_ _ _ _ _)
                (_ _ _ _ X)))

(define grid3 '((X)))

(define (get-column grid column_num)
  (cond
  [(empty? grid '())]
  [else (cons (list-ref (first grid) column_num) (get-column (rest grid) column_num))])
)