#lang racket

(require rackunit
         "motorway.rkt"
         "mw-find.rkt"
         "mw-car.rkt")

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
 "Recognise when to overtake?"
 (let* ([road (list '(mclaren 2 38) '(astra 1 60) '(reliant 1 38) '(skoda 1 22) '(mini 1 61) '(lorry 0 35) '(jag 0 28))]
        [the-jag (find-car 'jag road)]
        [the-astra (find-car 'astra road)]
        [the-skoda (find-car 'skoda road)]
        [the-lorry (find-car 'lorry road)]
        [the-reliant (find-car 'reliant road)]
        [the-mclaren (find-car 'mclaren road)])
   ;(display-road road)
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
   (check-false (can-overtake? the-lorry 5 road))
   ; reliant cannot overtake
   (check-false (can-overtake? the-reliant 4 road))
   ; mclaren cannot overtake
   (check-false (can-overtake? the-mclaren 10 road))
   ))

(test-case
 "Recognise when to move in?"
 (let* ([speed 1]
        [road (list '(mclaren 2 10) 
                    '(astra 1 19) 
                    '(reliant 1 16) 
                    '(skoda 1 10) 
                    '(mini 1 4) 
                    '(lorry 0 16) 
                    '(jag 0 4))])
   ; can move in: astra, skoda
   (for-each
    check-true 
    (map (lambda (x) 
           (can-move-in? x speed road)) 
         (filter 
          (lambda (x) 
            (or 
             (eq? 'astra (first x)) 
             (eq? 'skoda (first x)))) 
          road)))
   ; can't move in: everyone else
   (for-each
    check-false 
    (map (lambda (x) 
           (can-move-in? x speed road)) 
         (filter 
          (lambda (x) 
            (and 
             (not (eq? 'astra (first x))) 
             (not (eq? 'skoda (first x))))) 
          road)))
   
   ))