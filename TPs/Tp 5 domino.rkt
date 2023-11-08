;Interface Domino
;(define (make_domino a b)
;(define (make_domino a b)
;  (list a b))

(define creer_domino list)
(define premier car)
(define seconde cadr)

(define (est_double? d)
  (= (premier d) (second d)))
(define (retourner d)
  (creer_domino (seconde d) (premier d)))

(define (contient n d)
  (or (= n (car d)) (= n (cadr d))))

;Interface Jeu
(define creer_jeu list)

(define (premier_element_jeu j)
 (car j))
(define (suite_jeu j)
 (cdr j))
(define (neme_element_jeu n j)
  (cond ((jeu_vide? j) (display "Erreur !"))
        ((= n 1)(premier_element_jeu j))
     (else (neme_element_jeu (- n 1) (suite_jeu j)))))
(define (jeu_vide? j)
 (null? j))

(define (ajout_domino_jeu d j)
  (cons d j))
;Fin Interface
;Q5
(define (doubles j)
   (filter est_double? j))
;Q6
(define (peut_jouer? n j)
  (cond ((jeu_vide? j) #f)
        ((contient n (premier_element_jeu j)) #t)
        (else (peut_jouer? n (suite_jeu j)))))
;Q7
(define (extraire n j)
  (let ((d (premier_element_jeu j)))
    (if (contient n d)
        d
        (extraire n (suite_jeu j)))))
;Q8
(define (chaine_valide? ch)
  (cond ((jeu_vide? ch) #t)
        ((jeu_vide? (suite_jeu ch)) #t)
        (else (and (= (second (premier_element_jeu ch))
                      (premier (premier_element_jeu (suite_jeu ch))))
                   (chaine_valide? (suite_jeu ch))))))

(define (ext-g c)
  (premier (premier-chaine c)))

(define (ext-d c)
  (define (dernier-chaine c)
    (cond ((chaine-vide? c) ())
          ((chaine-vide? (suite-chaine c)) (premier-chaine c))
          (else (dernier-chaine (suite-chaine c)))))
  (second (dernier-chaine c)))

;Q10
(define (egal-domino? d1 d2)
  (and (= (premier d1) (premier d2))
       (= (second d1) (second d2))))

;Q11
(define (ajouter domino chaine)
(let ((dG (ext-g chaine)) (dD (ext-d chaine)))
(cond ((chaine-vide? chaine) (creer-chaine domnino))
((= dG (second domino)) (ajouter-domino-debut-jeu domino chaine))
((= dG (premier domino)) (ajouter-domino-debut-jeu (renverser domino) chaine))
((= dD (premier domino)) (ajouter-domino-debut-jeu domino chaine))
(else (ajouter-domino-debut-jeu (renverser (domino)) chaine)))))

;Q12

; pose prend comme paramètre une liste (j ch) composée d'un jeu j et d'une chaîne ch
; calcule la liste (jp chp) obtenue en ajoutant (si cela est possible) un domino d du jeu j à la chaîne ch.
; le cas échéant, jp est j duquel on a retiré le domino d, 
; et chp est ch à laquelle on a ajouté de manière cohérente le domino d.
; Dans le cas où aucun domino de j ne peut être ajouté à ch, le résultat est la liste (j ch)

(define (pose l)
  
  (define (action-jeu domino jeu chaine)
    (list (supprimer domino jeu) (ajouter domino chaine)))
  
  ; l est une liste jeu-chaine, on rend une liste jeu-chaine
  (let ((jeu (car l)) (chaine (cadr l)))
    (let ((g (ext-g chaine)) (d (ext-d chaine)))
      (cond ((peut-jouer g jeu)
             (action-jeu (extraire g jeu) jeu chaine))
            ((peut-jouer d jeu)
             (action-jeu (extraire d jeu) jeu chaine))
            (else l)))))

            
    (define (tracer-gros-point p)
  (draw-solid-disk p 2))

(define (tracer-rectangle p q)
  (begin (draw-solid-line p (make-posn (posn-x p) (posn-y q)))
         (draw-solid-line p (make-posn (posn-x q) (posn-y p)))
         (draw-solid-line q (make-posn (posn-x q) (posn-y p)))
         (draw-solid-line q (make-posn (posn-x p) (posn-y q)))))

(define (tracer-demi-dominos p n)
  (begin (tracer-rectangle (make-posn (- (posn-x p) 12) (- (posn-y p) 12)) 
                           (make-posn (+ (posn-x p) 12) (+ (posn-y p) 12)))
         (cond ((= n 1) (tracer-gros-point p))
               ((= n 2) (begin (tracer-gros-point (make-posn (- (posn-x p) 6) (+ (posn-y p) 6)))
                               (tracer-gros-point (make-posn (+ (posn-x p) 6) (- (posn-y p) 6)))))
               ((= n 3) (begin (tracer-gros-point (make-posn (- (posn-x p) 6) (+ (posn-y p) 6))) 
                               (tracer-gros-point (make-posn (+ (posn-x p) 6) (- (posn-y p) 6)))
                               (tracer-gros-point p)))
               ((= n 4) (begin (tracer-gros-point (make-posn (- (posn-x p) 6) (+ (posn-y p) 6))) 
                               (tracer-gros-point (make-posn (+ (posn-x p) 6) (- (posn-y p) 6)))
                               (tracer-gros-point (make-posn (- (posn-x p) 6) (- (posn-y p) 6)))
                               (tracer-gros-point (make-posn (+ (posn-x p) 6) (+ (posn-y p) 6)))))
               ((= n 5) (begin (tracer-gros-point (make-posn (- (posn-x p) 6) (+ (posn-y p) 6))) 
                               (tracer-gros-point (make-posn (+ (posn-x p) 6) (- (posn-y p) 6)))
                               (tracer-gros-point (make-posn (- (posn-x p) 6) (- (posn-y p) 6)))
                               (tracer-gros-point (make-posn (+ (posn-x p) 6) (+ (posn-y p) 6)))
                               (tracer-gros-point p)))
               ((= n 6) (begin (tracer-gros-point (make-posn (- (posn-x p) 6) (+ (posn-y p) 6))) 
                            (tracer-gros-point (make-posn (+ (posn-x p) 6) (- (posn-y p) 6)))
                            (tracer-gros-point (make-posn (- (posn-x p) 6) (- (posn-y p) 6)))
                            (tracer-gros-point (make-posn (+ (posn-x p) 6) (+ (posn-y p) 6)))
                            (tracer-gros-point (make-posn (- (posn-x p) 6) (posn-y p)))
                            (tracer-gros-point (make-posn (+ (posn-x p) 6) (posn-y p))))))))

(define (draw-dominos p d)
  (begin (tracer-demi-dominos (make-posn (- (posn-x p) 12) (posn-y p)) (premier d))
         (tracer-demi-dominos (make-posn (+ (posn-x p) 12) (posn-y p)) (second d))))

        



;Jeu de test
(define jeu '((1 2) (3 4) (2 2) (1 3) (3 3)))
(define chaine '((1 2) (2 3) (3 1) (1 4) (4 6)))
(define domino '(2 3))