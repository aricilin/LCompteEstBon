;Exo 15
(define bissextile?
  (lambda (annee)
    (or (= 0 (modulo annee 400))
        (and (= 0 (modulo annee 4))
             (not (= 0 (modulo annee 100)))))))
;Exo 16
(define nb-annees-bissextiles (lambda (annee)
          (if (= annee 1900)
              0
              (if (bissextile? annee)
                  (+ 1 (nb-annees-bissextiles (- annee 1)))
                  (nb-annees-bissextiles (- annee 1))))))
;Exo 17
(define nb-jours-au-1-jan
  (lambda (mois)
          (cond ((= 1 mois) 0)
                ((= 2 mois) 31)
                ((= 3 mois) (+ 31 28))
                ((= 4 mois) (+ 31 28 31))
                ((= 5 mois) (+ 31 28 31 30))
                ((= 6 mois) (+ 31 28 31 30 31))
                ((= 7 mois) (+ 31 28 31 30 31 30))
                ((= 8 mois) (+ 31 28 31 30 31 30 31))
                ((= 9 mois) (+ 31 28 31 30 31 30 31 31))
                ((= 10 mois) (+ 31 28 31 30 31 30 31 31 30))
                ((= 11 mois) (+ 31 28 31 30 31 30 31 31 30 31))
                ((= 12 mois) (+ 31 28 31 30 31 30 31 31 30 31 30))
                (else (display "Erreur")))))
         
(define nb-jours
  (lambda (jour mois annee)
    (+ (* (- annee 1900) 365)
       (nb-annees-bissextiles annee)
       (nb-jours-au-1-jan mois)
       (- jour 1)
       (if (and (bissextile? annee) (< mois 3)) -1 0))))

(define (jour n)
  (cond ((= n 0) (display "lundi"))
        ((= n 1) (display "mardi"))
        ((= n 2) (display "mercredi"))
        ((= n 3) (display "jeudi"))
        ((= n 4) (display "vendredi"))
        ((= n 5) (display "samedi"))
        ((= n 6) (display "dimanche"))))

(define (jour-de-semaine j m a)
  (jour (modulo (nb-jours j m a) 7)))
       


