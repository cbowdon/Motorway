#lang racket

(provide find-car
         find-ahead
         find-adjacent
         make-road
         make-car
         update-road
         update-car-list
         display-road)

; find a car's location on the road
(define (find-car id road)
  (let ([result (filter (lambda (x) (eq? (first x) id)) road)])
    (if [empty? result]
        #f
        (first result))))

; general finding function
(define (find-closest target-lane pos-pred this-car road)
  (let* ([this-pos (third this-car)]
         [cars-in-lane (filter (lambda (x) (and (equal? target-lane (second x)) (pos-pred this-pos (third x)))) road)])
    (sort cars-in-lane (lambda (a b) (< (abs (- (third a) this-pos)) 
                                        (abs (- (third b) this-pos)))))))

; get the cars ahead, ordered by proximity
(define (find-ahead this-car road)
  (find-closest (second this-car) < this-car road))

; get cars behind in adjacent lane, order by proximity
(define (find-adjacent this-car next-lane road)  
  (let* ([op (cond [(eq? next-lane 'in) -]
                   [(eq? next-lane 'out) +]
                   [else (error "That's not a lane!")])]
         [target-lane (op (second this-car) 1)])
    (if [or (< target-lane 0) (> target-lane 2)]
        #f
        (find-closest target-lane > this-car road))))

; call each car in car-list to get its id
; assign positions
(define (make-road car-list)
  (let ([ids (map (lambda (x) (first (x '()))) car-list)])
    (define (random-pos new-road)
      (let ([new-pos (list (random 2) (random 100))])
        (if [not (member new-pos (map rest new-road))]
            new-pos
            (random-pos new-road))))
    (define (new-road-iter ids-left road-so-far)
      (cond [(empty? ids-left) road-so-far]
            [else (new-road-iter (rest ids-left) (cons (cons (first ids-left) (random-pos road-so-far)) road-so-far))]))    
    (reverse (new-road-iter ids '()))))

; make a car
; speed is steps it will move in one turn
; overtake-distance  ...
; changes-lane ...
(define (make-car id speed overtake-distance moves-in)
  (lambda (road) 
    (let ([current (find-car id road)])
      (if [not current]
          (list id 0 0)
          (list id (second current) (+ (third current) speed))))))

; call all cars and create a new road
(define (update-road car-list road)
  road)

; remove out-of-scope cars
(define (update-car-list car-list road)
  car-list)

; pretty-print the road
(define (display-road road)
  (void))