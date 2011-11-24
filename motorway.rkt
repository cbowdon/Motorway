#lang racket

(require "mw-find.rkt"
         "mw-car.rkt")

(provide make-road
         update-road
         display-road)

; call each car in car-list to get its id
; assign positions
(define (make-road car-list)
  (let ([road-size 20]
        [ids (map (lambda (x) (first (x '()))) car-list)])
    (define (random-pos new-road)
      (let ([new-pos (list (random 3) (random (/ road-size 2)))])
        (if [not (member new-pos (map rest new-road))]
            new-pos
            (random-pos new-road))))
    (define (new-road-iter ids-left road-so-far)
      (cond [(empty? ids-left) road-so-far]
            [else (new-road-iter (rest ids-left) (cons (cons (first ids-left) (random-pos road-so-far)) road-so-far))]))    
    (reverse (new-road-iter ids '()))))

; call all cars and create a new road
(define (update-road car-list road)
  (map (lambda (x) (x road)) car-list))

(define (get-sorted-lane lane road)
  (let ([lane-number (cond [(eq? lane 'inside) 0]
                           [(eq? lane 'middle) 1]
                           [(eq? lane 'outside) 2])])
    (sort (filter (lambda (x) (equal? lane-number (second x))) road) (lambda (a b) (< (third a) (third b))))))  

; remove out-of-scope cars
(define (update-car-list car-list road)
  car-list)

; pretty-print the road
(define (display-road road)
  (let* ([road-size 20]
         [text (for/list ([i 3]) (make-vector road-size '_))])          
    (for-each (lambda (x) (vector-set! (first text) (third x) (first x))) (get-sorted-lane 'inside road))
    (for-each (lambda (x) (vector-set! (second text) (third x) (first x))) (get-sorted-lane 'middle road))
    (for-each (lambda (x) (vector-set! (third text) (third x) (first x))) (get-sorted-lane 'outside road))
    (printf "________________________________________________\n")
    (printf "|\tinside\t|\tmiddle\t|\toutside\t|\n")
    (printf "________________________________________________\n")    
    (for ([x (first text)]
          [y (second text)]
          [z (third text)])
      (printf "|\t~a\t|\t~a\t|\t~a\t|\n" x y z))))

