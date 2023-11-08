; car=> tete
; cdr => queue

;Exo 1
(cons (cons 1 2) (cons 3 4))
;reponse ((1 . 2) 3 . 4)
(cdr (cons 1 (cons 2 ())))
;reponse (2)
(list? (cons (cons 1 2) (cons 3 4)))
;reponse #f
(list? '(1 2 3))
;reponse #t
(pair? '(1 2 3))
;reponse #t
;tous les liste sauf la liste vide sont paire toujours
' (1 2 (3 4)"toto" ((5 6)))
'()
;Exo 2
'(9 (((8 4 5 (4) 25) 3) 7))
;reponse : (9 (((8 4 5 (4) 25) 3) 7))
(car ' (() ()))
;reponse : ()
(cdr ' (() ()))
; reponse : (())
'(56 ’p (’K 6) 8 (() (7 6 2)))
; reponse : (56 ’p (’K 6) 8 (() (7 6 2)))

;Exo 3
(car '(2))
; reponse : 2
(cdr '(2))
; reponse : ()
(define liste (list '(2) '((1 2) 3 4) '(() ()) '(1 (2 (3 4 5) 6) 7)'(((8 4) 6 (5 4)) '(7 8 9))))

;map ;reduce; filter

;cddr => queue de queue
;cdadr => queue de la tete de la queue
;cadar => tete de queue de tete
(display "----------Exo 4------------")
;Exo 4

;(+ ( 2 6) 10); Erreur
'(+ ( 2 6) 10) ; reponse : (+ (2 6) 10)
(car (car '((5 9) 7 5))) ; reponse : 5
(car (cdr '((5 9) 7 5))) ; reponse : 7
(cdr (car '((5 9) 7 5))) ; reponse : (9)
(cdr (cdr '((5 9) 7 5))) ; reponse : (5)
(cons '(a b) '(c d))     ; reponse : ((a b) c d)
(cons (+ 2 5) '(e f))    ; reponse : (7 e f)
(cons 'a '(b c d))       ; reponse : (a b c d)
(cons 'a '())            ;reponse :  (a)
(cons (car '(a b c)) (cdr '(a b c))) ;reponse :  (a b c)  
(let ((l '(a (b c e))))  
(if (equal? (cdr l) ()) 'yes 'nope)) ; reponse : nope

; caar => car(car (...
; cddr => cdr(cdr (...

;Exo 5
(define a (cons '+ (cons (cons ' * (cons (+ 2 6) (cons 3 ()))) (cons (+ 1 3) ()))))
; a = (+ (* 8 3) 4)
; cdr a = ((* 8 3) 4)
; car a = +
; cdadr a= (8 3) queue de tete de queue
; eval a= 28

;Exo 6
(define (premdeux? x l)
 (cond ((null? l) #f)
       ((equal? x (car l)) #t)
       ((null? (cdr l)) #f)
       ((equal? x (cadr l)) #t)
       (else #f)))

(list '1 '(+) '(2 3))

(append '(1 2 3) '(4 5 6) '(7))

;Exo 7
(cons '(a b) '(c d))
;((a b) c d)
(list '(a b) '(c d))
;((a b) (c d))
(append '(a b) '(c d))
;(a b c d)

;Exo 8
(append (cons 'a (cons 'b ())) (list (+ 2 3) 'a))
;(a b 5 a)
(+ 2 (cadar (list (cons 3 (list 7 (+ 2 4))) (+ 4 5))))
;(9)
(append (list (cons 'a 'b)) (cons 'c (list 'd)))
;((a . b) c d)
(list (cons 'a ()) 'b)
;((a) b)
(list (cons 'a (cons 'b ())) (list (+ 2 3) 'a))
;((a b) (5 a))
(* 2 (cadar (list (append (cons 3 (list 7 (+ 2 4))) (list (+ 4 5))))))
;(* 2 (cadar ((3 7 6 9))) -> 14

;Exo 9
(define (longeur l)
  (if (null? l)
      0
      (+ 1 (longeur (cdr l)))))


;Exo 10
(define (plusun n) (+ 1 n))

(map plusun '(1 2 3 4 5 6))

(define (doubleSansMap l)
  (if (null? l)()
        (cons (* 2 (car l)) (doubleSansMap(cdr l)))))



(define (doubleM l)
  (map (lambda (n) (* 2 n)) l));lambda ile fonksiyon yazmamiza gerek kalmiyor locale olarak tanimliyoruz

(define  (mymap f l)
  (if (null? l)
      ()
      (cons (f (car l)) (mymap f (cdr l)))))
;(mymap (lambda (n) (* 3 n)) '(1 3 2 3 4 ...))


