;TD8
;;;;;;;;;

;le rendu de monnaie
; Somme S a rendre
; pieces (le nombre de pieces et leurs valeurs)
;Si S=0 -> 1 seule facon de rendre la monnaie
;Si S<0 -> 0 facon, je ne peux pas rendre qqe chose de negatif
;Si S>0 et n=0 -> 0 facon de rendre la monnaie

;je suppose avoir n pieces : je considere la premiere piece de valeur d.
;il y a des solutions avec cette piece et des solutions sans
;1 er cas : il reste a rendre (S-d) avec n pieces
;2eme cas : il reste a rendre S avec n-1 pieces

(define (valeurPiecesEuro piece)
(cond ((= piece 1) 1)
((= piece 2) 2)
((= piece 3) 5)
((= piece 4) 10)
((= piece 5) 20)
((= piece 6 ) 50)
((= piece 7 ) 100)
((= piece 8 ) 200)
))

(define (rendreMonnaie somme nbSortesPieces valeurPiece)
(define (rendre somme n)
(cond ((= somme 0) 1)
((or (< somme 0) (= n 0)) 0)
(else (+ (rendre somme (- n 1)) (rendre (- somme (valeurPiece n)) n)))))
(rendre somme nbSortesPieces))

(define (rendreEuro somme)
(rendreMonnaie somme 8 valeurPiecesEuro))

;EXERCICE 1

(define (rendreMonnaieListe somme nbSortesPieces valeurPiece)
(define (rendre somme n rendu_en_cours);construit un rendu de monnaie
(cond ((= somme 0) (list rendu_en_cours)); ici : 
((or (< somme 0) (= n 0)) ()); cul de sac...mauvais choix
(else (append
       (rendre somme (- n 1)rendu_en_cours); la meme somme a rendre
       (rendre (- somme (valeurPiece n)); ajout d'une valeur a ma liste
               n
               (cons(valeurPiece n)rendu_en_cours))
        ))))
(rendre somme nbSortesPieces '()))

;let : creation un alias un identifiant <-> une expression
;(let ((a (+ 3 4))
;      (f (lambda (n) (+ 2 n)))
;      (g (lambda (n) (.... g(- n 1)...)

;EXERCICE 2

;on va choisir une paire. On aurait pu prendre un liste.
; la couleur est representee par un boolean : noir #t rouge #f

(define (make-noeud int col)
  (cons int col))

(define (get-noeud-int node)
  (car node))

(define (get-noeud-col node)
  (cdr node))

(define (noir? node)
  (get-noeud-col node))

(define (noeud-pair? node)
  (even? (get-noeud-int node)))
(define noir #t)
(define rouge #f)

(define n (make-noeud 4 rouge))
(define m (make-noeud 5 noir))

;-------------------INTERFACE--------------------

(define empty-arn ())
(define (make-arn node left right)
  (list node left right))
(define (get-root arn)
  (car arn))
(define (get-left arn)
  (cadr arn))
(define (get-right arn)
  (caddr arn))
(define (empty-arn? arn)
  (null? arn))

(define (leaf? arn)
  (and (not (empty-arn? arn))
       (empty-arn? (get-left arn))
       (empty-arn? (get-right arn))))
;EXERCICE 4
;rouge=#f si pair=vrai alors (qui vaut #f)
;noir=#t si pair=faux alors on veut noir (qui vaut #t)
(define (pairARN arbre)
   (if (empty-arn? arbre)
       empty-arn
       (let ((root (get-root arbre))
             (left (get-left arbre))
             (right (get-right arbre)))
         (make-arn
         (make-noeud (get-noeud-int root) (not (noeud-pair? root)))
         (pairARN left)
         (pairARN right)))))

;EXERCICE 5
;vide OK
;feuille OK
;si arbre gauche est monochrome et racine(gauche) a la meme couleur que la racine 
(define (monochrome? arbre)
  (if (or (empty-arn? arbre) (leaf? arbre))
      #t
      (or (and (monochrome? (get-left arbre))
               (equal? (get-noeud-col (get-root arbre))
                       (get-noeud-col (get-root (get-left arbre)))))
          (and (monochrome? (get-right arbre))
               (equal? (get-noeud-col (get-root arbre))
                       (get-noeud-col (get-root (get-right arbre))))))))
          

;TEST
  (define arbre (make-arn (make-noeud 5 rouge)
                          (make-arn (make-noeud 6 rouge) '() '())
                          (make-arn (make-noeud 8 noir) '() '())))
  