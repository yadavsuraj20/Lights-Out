#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(define n 5)
(define x0 0)
(define y0 0)

(define (make-2d-vector n init)
  (build-vector n (lambda (x) (make-vector n init)))) 
(define (2d-vector-set! vec i j val)
  (let*((v (vector-ref vec i)))
    (vector-set! v j val)))
(define (2d-vector-ref vec i j)
  (vector-ref (vector-ref vec i) j))

(define (vectors N)
  (cond [(= N 1) '(())]
        [else (append* (map (lambda (x) (append (list (cons 0 x)) (list (cons 1 x))))
                                     (vectors (- N 1))))]))

(define Mat (make-2d-vector n 0))
;(define (select-random N vec)
;  (let* ((no (expt 2 n)))
;    (if (= N 1) (vector (vector-ref vec (random no)))
;                (vector-append (vector (vector-ref vec (random no))) (select-random (- N 1) vec)))))
(define A (vector (vector 0 0 0 0 0)
                  (vector 0 0 0 0 0)
                  (vector 0 0 0 0 0)
                  (vector 0 0 0 0 0)
                  (vector 1 1 0 0 1)))

(define (toggle M i j)
  (begin (2d-vector-set! Mat i j (- 1 (2d-vector-ref Mat i j)))
         (2d-vector-set! M i j (- 1 (2d-vector-ref M i j)))
         (cond [(> i 0) (2d-vector-set! M (- i 1) j (- 1 (2d-vector-ref M (- i 1) j)))])
         (cond [(< i (- n 1)) (2d-vector-set! M (+ i 1) j (- 1 (2d-vector-ref M (+ i 1) j)))])
         (cond [(> j 0) (2d-vector-set! M i (- j 1) (- 1 (2d-vector-ref M i (- j 1))))])
         (cond [(< j (- n 1)) (2d-vector-set! M i (+ j 1) (- 1 (2d-vector-ref M i (+ j 1))))])))

;This function (min-buttons M) takes input in the form of vector-of-vectors and returns answer in form of vector-of-vectors which is
;the way of solving the puzzle in minimum number of moves.
(define (min-buttons M)
  (set! Mat (make-2d-vector n 0))
  (define (check-row i)     ;checks a row for a light which is on and toggles it off. 
    (define j 0)
    (define (chck j)
      (cond [(= j 5) #t]
            [(= (2d-vector-ref M i j) 1) (begin (toggle M (+ i 1) j) (chck (+ j 1)))]
            [else (chck (+ j 1))]))
    (chck 0))

  (define (play-forward)  ;checks all rows from 1 to 4  to toggle all lights off.
    (define i 0)
    (define (chck i)
      (cond [(= i 4) #t]
            [else (begin (check-row i) (chck (+ i 1)))]))
    (chck 0))

  (define (row1-after-forward)            ;Special cases of last row which tells whether puzzle is solvable or not 
    (let* ((vec (vector-ref M (- n 1))))  ;and helps in solving the puzzle
      (cond [(equal? vec (vector 0 0 0 0 0)) #t]
            [(equal? vec (vector 0 0 1 1 1)) (begin (toggle M 0 3) #t)]
            [(equal? vec (vector 0 1 0 1 0)) (begin (toggle M 0 1) (toggle M 0 4) #t)]
            [(equal? vec (vector 0 1 1 0 1)) (begin (toggle M 0 1) #t)]
            [(equal? vec (vector 1 0 0 0 1)) (begin (toggle M 0 3) (toggle M 0 4) #t)]
            [(equal? vec (vector 1 0 1 1 0)) (begin (toggle M 0 4) #t)]
            [(equal? vec (vector 1 1 0 1 1)) (begin (toggle M 0 2) #t)]
            [(equal? vec (vector 1 1 1 0 0)) (begin (toggle M 0 1) #t)]
            [else #f])))

  (define (play-forward-again)  ;repeats play-forward again but this time it will surely end in all lights turned off
    (play-forward))             ;if the puzzle is solvable.
  
  (begin (play-forward)
         (if (row1-after-forward) (begin (play-forward-again) Mat) "Not Solvable")))

;This function updates the whole grid time-to-time.
(define (draw-grid N)
  (define (color i j)   ;if (i,j) element of puzzle is 0 then black (off), else yellow (on).
    (let* ((vec (vector-ref A (- 4 i))))
      (if (= (vector-ref vec (- 4 j)) 0) "brown" "yellow"))) 
  (define (make-row i j)  ;makes one row of a grid.
    (cond [(= j 0) (rectangle 100 100 "solid" (color i j))]
          [else (overlay/xy (rectangle 100 100 "solid" (color i j)) 101 0 (make-row i (- j 1)))]))
  (define (draw-lights i)   ;makes the whole grid of lights.
    (cond [(= i 0) (make-row i 4)]
          [else (overlay/xy (make-row i 4) 0 101 (draw-lights (- i 1)))]))
  (draw-lights 4))

;This function takes the mouse event as input and then call other function to update the grid.
(define (mouse-event w x y me)
  (cond [(equal? me "button-down") (set! x0 (exact-floor (/ x 100)))
                                   (set! y0 (exact-floor (/ y 100)))
                                   (toggle A y0 x0)]))

;big-bang :- In this, the (to-draw) function is must and all others are optional which handles the inputs
; (keys and mouse etc.)
(big-bang 0 (to-draw draw-grid 505 505)
  (on-mouse mouse-event))