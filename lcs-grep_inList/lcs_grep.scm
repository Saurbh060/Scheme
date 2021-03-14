#lang scheme
 
(define (sublist l1 l2)
  (define (text-itr l1 l2)
  (define (full-match l1 l2)
    (cond ((null? l1) #t)
          ((null? l2) #f)
          ((eq? (car l1) (car l2)) (full-match (cdr l1) (cdr l2)))
          (else #f)))
  (cond ((null? l1) #t)
        ((null? l2) #f)
        ((full-match l1 l2) #t)
        (else (text-itr l1 (cdr l2)))))
            
  (if (null? l1)
      #t
      (text-itr l1 l2)))
 

(define (lgrep l1 l4)
  (cond ((null? l4) '())
        ((sublist l1 (car l4)) (cons (car l4)  (lgrep l1 (cdr l4))))
        (else (lgrep l1 (cdr l4)))))



;-------Q2--------------


(define (lcs l1 l2)
  (define (pick i s)
  (if (= i 1)
      (car s)
      (pick (- i 1) (cdr s))))

  (define (dp i j x y)
    (cond
      ((or (= i 0) (= j 0)) 0)
      ((eq? (pick i x) (pick j y)) (+ 1 (dp (- i 1) (- j 1) x y)))
      (else (max (dp (- i 1) j x y) (dp i (- j 1) x y)))))


  (define (concat l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (concat (cdr l1) l2) )))

  (define (make-2dlist x l)
    (if (null? l)
        '(())
        (cons (cons x (car l)) (make-2dlist x (cdr l)))))
      

  (define (all_sets l1 l2)
    (cond ((null? l1) '(()))
          ((null? l2) '(()))
          ((eq? (car l2) (car l1)) (make-2dlist (car l1) (all_sets (cdr l1) (cdr l2))))
          (else (concat (all_sets l1 (cdr l2)) (all_sets l2 (cdr l1))))))


  (define (printlcs l7)
    (cond ((null? l7) '())
          ((= (length (car l7)) (dp (length l1) (length l2) l1 l2)) (cons (car l7) (printlcs (cdr l7) )))
          (else (printlcs (cdr l7) ))))
  (remove-duplicates (printlcs (all_sets l1 l2)) ) )

