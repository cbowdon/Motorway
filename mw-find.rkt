#lang racket

(provide find-car
         find-ahead
         find-adjacent)

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

; get cars in adjacent lane, either ahead or behind, order by proximity
(define (find-adjacent this-car next-lane direction road)  
  (let* ([op (cond [(eq? next-lane 'in) -]
                   [(eq? next-lane 'out) +]
                   [else (error "That's not a lane:" next-lane)])]
         [pr (cond [(eq? direction 'ahead) <]
                   [(eq? direction 'behind) >]
                   [else (error "That's not a direction:" direction)])]
         [target-lane (op (second this-car) 1)])
    (if [or (< target-lane 0) (> target-lane 2)]
        #f
        (find-closest target-lane pr this-car road))))
