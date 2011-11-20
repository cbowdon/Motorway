#lang racket

(provide find-car
         make-road
         make-car
         update-road
         update-car-list
         display-road)

; find a car's location on the road
(define (find-car id road)
  (let ([result (filter (lambda (x) (eq? (car x) id)) road)])
    (if [empty? result]
        #f
        (car result))))

; call each car in car-list to get its id
; assign positions
(define (make-road car-list)
  (let ([ids (map (lambda (x) (car (x '()))) car-list)])
    (define (random-pos new-road)
      (let ([new-pos (list (random 2) (random 100))])
        (if [not (member new-pos (map cdr new-road))]
            new-pos
            (random-pos new-road))))
    (define (new-road-iter ids-left road-so-far)
      (cond [(empty? ids-left) road-so-far]
            [else (new-road-iter (cdr ids-left) (cons (cons (car ids-left) (random-pos road-so-far)) road-so-far))]))    
    (reverse (new-road-iter ids '()))))

; make a car
; speed is steps it will move in one turn
; overtake-distance  ...
; changes-lane ...
(define (make-car id speed overtake-distance changes-lane)
  (lambda (road) 
    (let ([current (find-car id road)])
      (if [not current]
          (list id 0 0)
          (list id (cadr current) (+ (caddr current) speed))))))

; call all cars and create a new road
(define (update-road car-list road)
  road)

; remove out-of-scope cars
(define (update-car-list car-list road)
  car-list)

; pretty-print the road
(define (display-road road)
  (void))