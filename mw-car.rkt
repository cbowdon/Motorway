#lang racket

(require "mw-find.rkt")

(provide make-car
         should-overtake?
         can-overtake?
         should-move-in?
         can-move-in?)

; make a car
; speed is steps it will move in one turn
; overtake-distance  ...
; changes-lane ...
(define (make-car id speed overtake-distance moves-in)
  (lambda (road) 
    (let ([this-car (find-car id road)])
      (if [not this-car]
          (list id 0 0)
          (list id (second this-car) (+ (third this-car) speed))))))

; decide if overtaking is necessary
; condition: dist to car ahead < speed
(define (should-overtake? this-car road speed)
  (let ([cars-ahead (find-ahead this-car road)])
    (and (not (empty? cars-ahead)) 
         (<= (third (first cars-ahead)) (+ (third this-car) speed)))))

; generic lane changing
(define (can-change-lane? this-car road lane forward-gap backward-gap)
  (let ([cars-adj-ahead (find-adjacent this-car lane 'ahead road)]
        [cars-adj-behind (find-adjacent this-car lane 'behind road)])
    (and
     (list? cars-adj-behind)
     (or (empty? cars-adj-ahead)
         (> (- (third (first cars-adj-ahead)) (third this-car)) forward-gap))
     (or (empty? cars-adj-behind)
         (> (- (third this-car) (third (first cars-adj-behind))) backward-gap)))))

; decide if overtaking is safe
; condition: adjacent-lane car dist is > 2 x speed (if behind) and < speed (if ahead)
(define (can-overtake? this-car road speed)
  (can-change-lane? this-car road 'out speed (* 2 speed)))

; decide if moving in is necessary
(define (should-move-in? this-car road tendency)
  (cond [(equal? (second this-car) 0) #f]
        [(equal? (second this-car) 1) tendency]
        [(equal? (second this-car) 2) #t]))

; decide if moving in is safe
; condition: adjacent-lane car dist is > 2 x speed (if ahead) and < speed (if behind)
(define (can-move-in? this-car road speed) 
  (can-change-lane? this-car road 'in (* 2 speed) speed))
