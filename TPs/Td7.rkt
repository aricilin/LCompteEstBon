;TD 7

;Exercice 1

(define (faire_complexe a b)
  (list a b))

(define (partie_reel c)
  (car c))

(define (partie_imag c)
  (cadr c))

(define (reel? c)
   (= 0 (partie_imag c)))

(define (imag? c)
   (= 0 (partie_reel? c)))

;utilisation de l'interface

(define (somme_complexe z c)
  (let ((zr(partie_reel z))
        (zi(partie_imag z))
        (cr (partie_reel c))
        (ci (partie_imag c)))
      (faire_complexe (+ cr zr) (+ zi ci))))

(define (produit_complexe z c)
  (let ((zr(partie_reel z))
        (zi(partie_imag z))
        (cr (partie_reel c))
        (ci (partie_imag c)))
      (faire_complexe (- (* cr zr) (* ci zi))
                      (+ (* cr zi)(* ci zr)))))

(define (inverse_complexe z)
   (let* ((zr(partie_reel z))
         (zi(partie_imag z))
         (d (+ (* zr zr) (* zi zi))))
     (if (= d 0)
         (display "IMPOSSIBLE : division par zero")
  (faire_complexe (/ zr d) (/ (- zi) d)))))

(define (division_complexe z c)
  (produit_complexe z (inverse_complexe c)))

(define (puissance z n)
(define (puissance_term z n acc)
  (if (= n 0)
      acc
      (puissance_term z (- n 1) (produit_complexe z acc))))
(if (>= n 0)
  (puissance_term z n (faire_complexe 1 0))
  (puissance_term (inverse_complexe z) (- n) (faire_complexe 1 0))))

;TD 7 Exercice 4

(define (monome deg coef) ;4x -> (0 4) ; 4x
   (if (= deg 0)
       (list coef)
       (cons 0 (monome (- deg 1) coef))))

(define (addpoly p1 p2)
  (cond ((null? p1) p2)
        ((null? p2) p1)
        (else (cons (+ (car p1) (car p2))
                      (addpoly (cdr p1) (cdr p2))))))

(define (multconst p c)
  (map (lambda (x) (* c x)) p))

;(map f L) -> (f(L0) f(L1) f(L3) ....)

(define (multvar p)
  (cons 0 p))

(define (evalpoly p v) ; 1+3x^2-7x^3  a evaluer en v= -2 x yerine -2 iste
  (if (null? p)
      0
      (+ (car p) (* v (evalpoly (cdr p) v)))))

(define (evalpoly_term p v acc)
  (if (null? p)
      acc
      (evalpoly_term (multconst (cdr p) v) v (+ (car p) acc))))

;Exercice 5
(define (monome->string coef deg)
  (if (= coef 0)
      ""
  (begin
    (if (> coef 0)
        (begin (display "+") (display coef))
    (display coef))
    (cond ((= deg 1) (display "x"))
          ((> deg 1) (begin (display "x^")
                            (display deg)))))))
;Exercice 6
(define (polynome->string p)
  (define (affiche p n)
     (if (null? (cdr p))
         (monome->string (car p) n)
         (begin
           (monome->string (car p) n)
           (affiche (cdr p) (+ n 1)))
         ))
    (affiche p 0))

(polynome->string '(1 0 3 4))

;Exercice 7

(define (multpoly p1 p2)
  (if (null? p1)
      '()
      (addpoly (multconst p2 (car p1)) (multvar (multpoly (cdr p1) p2)))))