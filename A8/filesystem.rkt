;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname filesystem) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Put your header here

(require "filesystem-lib.rkt")

;; A Filesystem Object (FSObject) is one of:
;; * File
;; * Dir

(define-struct file (name size owner))
;; A File is a (make-file Str Nat Sym)

(define-struct dir (name owner contents))
;; A Dir is a (make-dir Str Sym (listof FSObject))


(define example-fs
  (make-dir "root" 'root
      (list
         (make-dir "Dan" 'dan
              (list (make-file "log.txt" 768 'dan)
                    (make-file "profile.jpg" 60370 'dan)
                    (make-dir "music" 'dan
                        (list (make-file "Thelonius Monk.mp3" 92227584 'dan)))))
         (make-dir "Slides" 'teaching
              (list (make-dir "cs135" 'teaching
                        (list (make-file "01-intro.pdf" 72244 'teaching)
                              (make-file "11-trees.pdf" 123124 'teaching)
                    (make-dir "system" 'root
                        (list (make-dir "logs" 'teaching empty)))))))
                              (make-file "vmlinuz" 30 'root))))

;; B
(define (fsobject-name FSObject)
  (cond [(file? FSObject) (file-name FSObject)]
        [(dir? FSObject) (dir-name FSObject)]))

(check-expect (fsobject-name (make-file "hello.txt" 32 'Dan))
"hello.txt")
(check-expect (fsobject-name (make-dir "My Music" 'nobody empty))
"My Music")

;; C

(define (count-files-dir dir)
  (cond
     [(empty? dir) 0]
     [else (+ (count-files (first dir))
              (count-files-dir (rest dir)))]))

(define (count-files FSObject)
  (cond [(file? FSObject) 1]
        [else (count-files-dir (dir-contents FSObject))]))

(check-expect (count-files (make-file "README" 16 'me)) 1)
(check-expect (count-files example-fs) 6)

;; D


(define (file-check fso lst)
  (cond [(and (string=? (file-name fso) (first lst))
              (empty? (rest lst))) true]
        [else false]))
(define (con-check lf lst)
  (cond [(or (empty? lst) (empty? lf)) false]
        [(and (file? (first lf)) (empty? (rest lst)))
         (or (file-check (first lf) lst)
             (con-check (rest lf) lst))]
        [(dir? (first lf)) (or (dir-check (first lf) lst)
                               (con-check (rest lf) lst))]
        [else (con-check (rest lf) lst)]))
(define (dir-check fso lst)
  (cond [(string=? (dir-name fso) (first lst))
         (con-check (dir-contents fso) (rest lst))]
        [else false]))

(define (file-exists? fso lst)
  (cond [(empty? fso) false]
        [(file? fso) (file-check fso lst)]
        [(dir? fso) (dir-check fso lst)]))
    
(check-expect (file-exists? example-fs (list "root" "Dan")) false)
(check-expect (file-exists? example-fs (list "readme.txt")) false)

;; E


    
;; F

(define (disk-hog fsobject)
  (local [(define (disk-hog/acc lof max-so-far)
            (cond
              [(empty? lof) (file-owner max-so-far)]
              [(> (file-size (first lof)) (file-size max-so-far))
               (disk-hog/acc (rest lof) (first lof))]
              [else (disk-hog/acc (rest lof) max-so-far)]))
          (define (file-list-creator fsobject)
             (local [(define (file-list-creator/ lofso)
                      (cond
                        [(empty? lofso) empty]
                        [(file? (first lofso))
                         (cons (first lofso) (file-list-creator/ (rest lofso)))]
                        [(dir? (first lofso))
                         (append
                          (file-list-creator/ (dir-contents (first lofso)))
                          (file-list-creator/ (rest lofso)))]))]
              (cond
                [(file? fsobject) (list fsobject)]
                [(dir? fsobject)
                 (file-list-creator/ (dir-contents fsobject))])))]
      (cond
        [(empty? (file-list-creator fsobject)) false]
        [else (disk-hog/acc (file-list-creator fsobject)
                            (make-file "a" 0 "b"))])))

(check-expect (disk-hog example-fs) 'dan)
(check-expect (disk-hog (make-dir "secrets" 'cia empty)) false)


;; G



