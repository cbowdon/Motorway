#lang racket

(require rackunit
         "motorway.rkt")

(test-case
 "Find a car on a road"
 (let* ([road (list '(skoda 1 60) '(mini 0 59) '(lorry 0 35) '(jag 0 10) '(astra 1 2))]
        [the-jag (find-car 'jag road)]
        [the-astra (find-car 'astra road)]
        [the-skoda (find-car 'skoda road)])
   ; find our car
   (check-equal? the-jag '(jag 0 10))
   ; find cars ahead of us
   (check-equal? (find-ahead the-jag road) (list '(lorry 0 35) '(mini 0 59)))
   ; one car in the adjacent lane to the jaaaaag
   (check-equal? (find-adjacent the-jag 'out 'behind road) (list '(astra 1 2)))
   ; no cars adjacent to the astra 
   (check-equal? (find-adjacent the-astra 'out 'behind road) '())
   (check-equal? (find-adjacent the-astra 'in 'behind road) '())
   ; a mini is inside to the skoda
   (check-equal? (find-adjacent the-skoda 'in 'behind road) (list '(mini 0 59) '(lorry 0 35) '(jag 0 10)))
   ; the other adjacent lane is not okay
   (check-false (find-adjacent the-jag 'in 'behind road))
   ;there are cars adjacent but ahead of the jag
   (check-equal? (find-adjacent the-jag 'out 'ahead road) (list '(skoda 1 60)))
   ))

(test-case
 "Making a car"
 (let ([jag (make-car 'jag 1 1 #t)]
       [road (list '(skoda 2 60) '(lorry 1 2) '(jag 1 35))])
   ; returns a procedure
   (check-true (procedure? jag))
   ; procedure responds to input by producing id & coords
   (check-equal? (length (jag road)) 3)
   (check-eq? (first (jag road)) 'jag)
   (check-equal? (map number? (rest (jag road))) (list #t #t))))

(test-case
 "Making a road - check the randomizer by looping"
 (define (the-test count)
   (unless [equal? count 0]     
     (let* ([jag (make-car 'jag 1 1 #t)]
            [skoda (make-car 'skoda 1 1 #t)]
            [lorry (make-car 'lorry 1 1 #t)]
            [car-list (list jag skoda lorry)]
            [road (make-road car-list)])
       ; three cars on the road
       (check-equal? (length road) 3)
       ; ids check-out
       (check-equal? (map car road) '(jag skoda lorry))
       ; no duplicate positions
       (check-equal? (length road)
                     (length (remove-duplicates (map rest road)))))
     (the-test (- count 1))))
 (the-test 1000))

(test-case
 "Cars normally drive in a straight line"
 (let* ([speed 5]
        ; i'll be taking the jaaaag
        [jag (make-car 'jag speed 1 #t)]
        ; the only car on the road
        [car-list (list jag)]
        ; init road
        [road (make-road car-list)]
        ; where's the jag start?
        [jag-lane (second (find-car 'jag road))]       
        [jag-pos (third (find-car 'jag road))])
   ; is it in the same lane?
   (check-equal? jag-lane (second (jag road)))
   ; has it moved forward?
   (check-true (> (third (jag road)) jag-pos))
   (check-equal? (third (jag road)) (+ jag-pos speed))))

(test-case
 "Update road... updates the road"
 (let* (; i'll be taking the jaaaag
        [jag-speed 5]
        [jag (make-car 'jag jag-speed 1 #t)]
        ; no let's take the rolls
        [rolls-speed 4]
        [rolls (make-car 'rolls rolls-speed 1 #t)]
        ; the only two cars on the road
        [car-list (list jag rolls)]
        ; init road
        [road (make-road car-list)]
        ; original car positions
        [j1 (find-car 'jag road)]
        [r1 (find-car 'rolls road)]
        ; update road
        [updated-road (update-road car-list road)]
        ; new car positions
        [j2 (find-car 'jag updated-road)]
        [r2 (find-car 'rolls updated-road)])
   ; cars in same order
   (for-each (lambda (x y) (check-equal? (first x) (first y))) road updated-road)
   ; cars in lane +/- 1
   (for-each (lambda (x y) (check-true (> 1 (abs (- (second x) (second y)))))) road updated-road)
   ; cars all advanced
   (for-each (lambda (x y) (check-true (< (third x) (third y)))) road updated-road)))

(test-case
 "Recognise when to overtake?"
 (let* ([road (list '(astra 1 60) '(reliant 1 38) '(skoda 1 22) '(mini 1 61) '(lorry 0 35) '(jag 0 33))]
        [the-jag (find-car 'jag road)]
        [the-astra (find-car 'astra road)]
        [the-skoda (find-car 'skoda road)]
        [the-lorry (find-car 'lorry road)])
   (display-road road)
   ; jag should wish to overtake lorry
   (check-true (should-overtake? the-jag 7 road))
   ; skoda has no desire to overtake astra
   (check-false (should-overtake? the-skoda 6 road))
   ; no one for lorry to overtake
   (check-false (should-overtake? the-lorry 5 road))
   ; jag cannot overtake the lorry because of the skoda
   (check-false (can-overtake? the-jag 7 road))
   ; astra is free to overtake the mini
   (check-true (can-overtake? the-astra 7 road))
   ; skoda can overtake (though he doesn't want to)
   (check-true (can-overtake? the-skoda 5 road)) 
   ; lorry cannot overtake (and doesn't want to)
   (check-false (can-overtake? the-lorry 5 road))))
  
 







