#lang racket

(require rackunit
         "motorway.rkt"
         "mw-find.rkt"
         "mw-car.rkt")

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
   (check-equal? (find-adjacent the-jag 'out 'ahead road) (list '(skoda 1 60)))))

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
 "Just damn try it"
 (define car-list (list (make-car 'audi 0 1 #f)
                        (make-car 'bmw 1 1 #f)
                        (make-car 'citroen 2 1 #f)
                        (make-car 'daimler 3 1 #f)
                        (make-car 'e-type 4 1 #f)
                        (make-car 'fiesta 5 1 #f)
                        (make-car 'golf 6 1 #f)))
 
 (define road (make-road car-list))
 
 (define (road-iter count car-list road)
   (when [> count 0]
     (display-road road)
     (road-iter (- count 1) car-list (update-road car-list road))))
 
 
 (road-iter 3 car-list road))



