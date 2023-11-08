(define LVal '(1 2 3 4 5 6 7 8 9 10 25 50 75 100))
(define Op '(+ * - /))
;Exercice
(define (make-cible)
  (+ 100 (random 900)))

;;; fonction qui renvoie le n-eme element d'une liste
(define (neme n l)
  (if (= n 0)
      (car l)
      (neme (- n 1) (cdr l))))

(define (tire-plaque)
  (neme (random (length LVal)) LVal))

(define (tire-n-plaques n)
  (if (= n 0)
      '()
      (cons (tire-plaque) (tire-n-plaques (- n 1)))))

(define (make-tirage)
  (tire-n-plaques 6))

;Exo 2
(define (estDans? x l)
  (cond ((null? l) #f)
        ((equal? x (car l)) #t)
        (else (estDans? x (cdr l)))))

;Exo 3
(define (estValide? op a b)
  (and (not (= 0 b))
       (let ((res ((eval op) a b)))
         (and (< 0 res) (integer? res)))))

;Exo 4
(define (opere op a b)
  (cond ((null? op)  '())
        ((estValide? (car op) a b) (cons ((eval (car op)) a b) (opere (cdr op) a b)))
        ((estValide? (car op) b a) (cons ((eval (car op)) b a) (opere (cdr op) a b)))
        (else (opere (cdr op) a b))))

;Exo 5
; il faut generer toutes les paires issues d'une liste...
(define (liste-paires l)
  (if (null? (cddr l)) (cons (car l) (cadr l))
      (append
       (map (lambda (x) (cons (car l) x)) (cdr l))
       (liste-paires (cdr l)))))
  