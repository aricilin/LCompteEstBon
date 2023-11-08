(require racket/trace)
;Exo 1
(define (fact n)
    (if (= n 0)
        1
        (* n (fact (- n 1)))))
(trace fact)
(fact 5)

;Exo 2
(define (fact-acc n acc)
  (if (= n 0)
      acc
      (fact-acc (- n 1) (* n acc))))
(trace fact-acc)
(fact-acc 5 1)

;Exo 3
(define (sum-squares-l-n n); reccurence enveloppe cunku islemler birikiyor kendi icinde 
  (if (= n 1)
      1
      (+(* n n) (sum-squares-l-n (- n 1)))))
(trace sum-squares-l-n)

;Exo 3.1
(define(sum_squares_iter n acc)
  (if (= n 1)
      acc
      (sum_squares_iter (- n 1) (+ acc (* n n) ))))

(sum_squares_iter 4 1)

;Exo 4
(define (inverse l)
  (if (null? l)
        ()
        (append (inverse (cdr l))(list (car l)))))

;recursive terminale ;(append (append (append () (5)) (4)) (3)
(define (inverse_iter l acc)
  (if (null? l)
      acc
      (inverse_iter (cdr l) (cons(car l) acc))))


;Exercice 5

 (define (sommeListe l)
   (if (null? l)
       0
       (+ (car l) (sommeListe (cdr l)))))

 (define (somme_iter l acc)
   (if (null? l)
       acc
       (somme_iter (cdr l) (+ acc (car l)))))

 ; Exercice 6
 ;A(4,2)
 (define (SRI l) ; '(5)
   (cond ((null? l) 0)
         ((null? (cdr l)) (car l))
         (else (+ (car l) (SRI (cddr l))))))

 (define (SRI_iter l acc)
   (cond ((null? l) acc); 0----> 0+acc
         ((null? (cdr l)) (+ (car l) acc)) ;(car l) ---> (car l) + acc
         (else (SRI_iter (cddr l)(+ (car l) acc)))))

;Exercice 7
 (define (appartient? m l)
   (cond ((null? l) #f)
         ((equal? (car l) m) #t);equal olursa harfler de olur = olursa sadece sayilar
         (else (appartient? m (cdr l))))); recursive terminale cunku baska islem yapilmiyor

 ;Exercice 8
 ;recursive enveloppe 
 (define (debut l long)
   (if (or(null? l) (= long 0))
          ()
         (cons  (car l) (debut (cdr l) (- long 1)))))
;recursive envelopper
 ;(calcul n(f(- n 1)))
 ;(calcul (car l) (f (cdr l)))

;(recursive terminal
 ;(f (- n 1) acc)
 
 ;Exercice 8 avec recursive terminale
 (define (debut2 l long acc)
   (if (or(null? l) (= long 0))
          acc
         (debut2  (cdr l) (- long 1) (append acc(list (car l))))))

 