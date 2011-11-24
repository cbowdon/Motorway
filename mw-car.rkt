#lang racket

(require "mw-find.rkt")

(provide make-car
         should-overtake?
         can-overtake?)

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

; decide if overtaking is necessary
; condition: dist to car ahead < speed
(define (should-overtake? this-car speed road)
  (let ([cars-ahead (find-ahead this-car road)])
    (and (not (empty? cars-ahead)) 
         (< (third (first cars-ahead)) (+ (third this-car) speed)))))

; decide if overtaking is safe
; condition: adjacent-lane car dist is > 2 x speed (if behind) and < speed (if ahead)
(define (can-overtake? this-car speed road)
  (let ([cars-adj-ahead (find-adjacent this-car 'out 'ahead road)]
        [cars-adj-behind (find-adjacent this-car 'out 'behind road)])
    (and
     (or (empty? cars-adj-behind)
         (> (- (third this-car) (third (first cars-adj-behind))) (* 2 speed)))
     (or (empty? cars-adj-ahead)
         (> (- (third (first cars-adj-ahead)) (third this-car)) speed)))))


