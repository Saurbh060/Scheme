#lang sicp

(define harry '())
(define voldemort '())

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b) (cons a (delay b)))))


(define (play s1 s2 m1 r)
  (let ((a1 0)  (a2 0) (a3 0)
       (b1 0)  (b2 0) (b3 0)
       (c1 0) (c2 0))
      (define (who-win player1 player2)
        (cond ((and (= player1 1) (= player2 3)) 1)
              ((and (= player1 2) (= player2 1)) 1)
              ((and (= player1 3) (= player2 2)) 1)
              ((and (= player2 1) (= player1 3)) 2)
              ((and (= player2 2) (= player1 1)) 2)
              ((and (= player2 3) (= player1 2)) 2)
              ((= player1 player2) 0)))
        
      (define (next-win-move move)
        (cond ((= move 1) 2)
              ((= move 2) 3)
              (else 1)))
        
      (define (max-move a b c)
        (cond ( (and (> a b) (> a c)) 1)
              ( (and (> b a) (> b c)) 2)
              (else 3)))
        
      (define (inc-count1 p)
        (cond ((= 1 p) (set! a1 (+ a1 1)))
              ((= 2 p) (set! a2 (+ a2 1)))
              (else (set! a3 (+ a3 1)))))
        
      (define (inc-count2 p)
        (cond ((= 1 p) (set! b1 (+ b1 1)))
              ((= 2 p) (set! b2 (+ b2 1)))
              (else (set! b3 (+ b3 1)))))
        
      (define (who-win-game harry voldemort r)
        (if (= r 0)
            (cond ((> c1 c2) 1)
                  ((< c1 c2) 2)
                  (else 0))
            (begin
              (cond ((= (who-win (stream-car harry) (stream-car voldemort)) 1) (set! c1 (+ c1 1)))
                    ((= (who-win (stream-car harry) (stream-car voldemort)) 2) (set! c2 (+ c2 1))))
              (who-win-game (stream-cdr harry) (stream-cdr voldemort) (- r 1)))))

      (define (cond1)
        (define h (cons-stream m1 v))   
        (define v (cons-stream 1  h))
        (set! harry h )
        (set! voldemort v))

      (define (cond2)
        (define (next-move h)
          (begin
            (inc-count1 (stream-car h))
            (cons-stream (next-win-move (max-move a1 a2 a3)) (next-move (stream-cdr h)))))
        
        (define h (cons-stream m1 v))
        (define v (cons-stream 1 (next-move h)))
        (set! harry h)
        (set! voldemort v))
      
      (define (cond3)
        (define (next-move v)
          (begin
            (inc-count2 (stream-car v)) 
            (cons-stream (next-win-move (max-move b1 b2 b3)) (next-move (stream-cdr v)))))

        (define v (cons-stream 1 h))
        (define h (cons-stream m1 (next-move v)))
        (set! harry h)
        (set! voldemort v))
      
      (define (cond4)
        (define (next-move1 v)
          (begin
            (inc-count2 (stream-car v))
            (cons-stream (next-win-move (max-move b1 b2 b3)) (next-move1 (stream-cdr v)))))
        (define (next-move2 h)
          (begin
            (inc-count1 (stream-car h))
            (cons-stream (next-win-move (max-move a1 a2 a3)) (next-move2 (stream-cdr h)))))
        
        (define h (cons-stream m1 (next-move1 v)))
        (define v (cons-stream 1 (next-move2 h)))
        (set! harry h)
        (set! voldemort v))
      
      (cond ((and (= s1 1) (= s2 1)) (cond1))
            ((and (= s1 1) (= s2 2)) (cond2))
            ((and (= s1 2) (= s2 1)) (cond3))
            ((and (= s1 2) (= s2 2)) (cond4)))
    (who-win-game harry voldemort r)))





;(define (stream-ref s n)
;  (if (= n 0)
;      (stream-car s)
;      (stream-ref (stream-cdr s) (- n 1))))

;(stream-ref harry 0)




;(stream-cdr harry)

;harry

























