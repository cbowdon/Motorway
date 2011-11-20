#lang racket

(require rackunit
         "motorway.rkt")

(test-case
 "Find a car on a road"
 (let ([road (list '(skoda 2 60) '(lorry 1 2) '(jag 1 35))])
   (check-equal? '(jag 1 35) (find-car 'jag road))))

(test-case
 "Making a car"
 (let ([jag (make-car 'jag 1 1 #t)]
       [road (list '(skoda 2 60) '(lorry 1 2) '(jag 1 35))])
   ; returns a procedure
   (check-true (procedure? jag))
   ; procedure responds to input by producing id & coords
   (check-equal? (length (jag road)) 3)
   (check-eq? (car (jag road)) 'jag)
   (check-equal? (map number? (cdr (jag road))) (list #t #t))))

(test-case
 "Making a road"
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
                (length (remove-duplicates (map cdr road))))))
  
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
        [jag-lane (list-ref (find-car 'jag road) 1)]       
        [jag-pos (list-ref (find-car 'jag road) 2)])
   ; is it in the same lane?
   (check-equal? jag-lane (cadr (jag road)))
   ; has it moved forward?
   (check-true (> (caddr (jag road)) jag-pos))
   (check-equal? (caddr (jag road)) (+ jag-pos speed))))




