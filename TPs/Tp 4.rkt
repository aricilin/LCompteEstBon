(define LVal '(1 2 3 4 5 6 7 8 9 10 25 50 75 100))
(define Op '(+ * - /))                           

; tirage de la plaque numéro n
(define (prendre n l)
  (if (= n 1) (car l)
      (prendre (- n 1) (cdr l))))

(define (tirage N)
  (if (= 0 N)
      ()
      (cons (prendre (+ 1 (random 14)) LVal) (tirage (- N 1)))))

(define (make-tirage)
  (tirage 6))

; nombre à atteindre
(define (make-cible)
  (+ 100 (random 900)))

; test d'appartenance
(define (estDans? e l)
  (cond ((null? l) #f)
        ((= e (car l)) #t)
        (else (estDans? e (cdr l)))))

; validité de calcul
(define (estValide? op a b)
  (if (= 0 b) #f
      (let ((resultat ((eval op) a b)))
        (and (< 0 resultat) (integer? resultat)))))

; opérations valides pour une paire de plaques
(define (opere lop a b)
  (cond ((null? lop) ())
        ((estValide? (car lop) a b) (cons ((eval (car lop)) a b) (opere (cdr lop) a b)))
        ((estValide? (car lop) b a) (cons ((eval (car lop)) b a) (opere (cdr lop) a b)))
        (else (opere (cdr lop) a b))))

; construction des plaques valides pour une paire d'elts que j'ajoute à une liste en construction
(define (pairesValides lop a b le)
  (letrec ((lov (opere lop a b))
           (iter (lambda (l)
                   (if (null? l)
                       ()
                       (cons (cons (car l) le) (iter (cdr l)))))))
    (iter lov)))
                       
; construction des plaques valides pour un elt et la liste des candidats possibles pour former une paire
(define (eltValide lop a le)
  (if (null? le) () ; pas d'element pour former une paire
      (append (pairesValides lop a (car le) (cdr le))
              (map (lambda (l) (cons (car le) l)) (eltValide lop a (cdr le))))))

; constrution des jeux de plaques valides pour un jeu de plaque donné
(define (plaqueValide lop le)
  (if (null? le) ()
      (append (eltValide lop (car le) (cdr le))
              (map (lambda (l) (cons (car le) l)) (plaqueValide lop (cdr le))))))

; construction des listes de jeux valides à partir de jeux valides
(define (jeuValide lop lj)
  (if (null? lj) ()
      (append (plaqueValide lop (car lj)) (jeuValide lop (cdr lj)))))

(define (trouve lp e)
  (cond ((null? lp) #f)
        ((estDans? e (car lp)) #t)
        (else (trouve (cdr lp) e))))

(define (compteEstBon_aux lop lp cible)
  (cond ((null? lp) (display "le compte n'est pas bon"))
        ((trouve lp cible) (display "le compte est bon"))
        (else (compteEstBon_aux lop (jeuValide lop lp) cible))))

(define (compteEstBon jeu cible)
  (compteEstBon_aux Op (list jeu) cible))
; (+ 10 75 9 100 75 7)

