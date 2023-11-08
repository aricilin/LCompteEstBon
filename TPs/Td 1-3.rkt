;— f : x 7→ x^3

(define f
(lambda (x)
(* x x x)))
;— g : y 7→ 3y + 7
(define g
(lambda (x)
(+ (* 3 x) 7)))
;— h : z 7→ 3f(z) + g(z) + 1
(define h
(lambda (x)
(+ (* 3 (f x)) (g x) 1)))
;— La fonction constante k2 : x 7→ 2.
(define k2 (lambda (x) 2))
;— La fonction identité id : x 7→ x.
(define id (lambda (x) x))
;— La fonction proj2 : (x, y) 7→ y.
(define proj2 (lambda (x y) y))
;— La fonction einstein : (u, v) → u+v 1+ uv c2 avec c = 300000
(define einstein
(lambda (u v)
(\ (+ u v) (+ 1 (/ (* u v) (* 300000 300000))))))


(define median
(lambda (x y z)
(if (< x y)
(if (< y z)
y
(if (< x z) z x))
(if (< x z)
x
(if (< y z) z y)))))

(define triangle?
(lambda (a b c)
(and (<= a (+ b c)) (<= b (+ a c)) (<= c (+ a b)))))

(define equilateral?
(lambda (a b c)
(and (triangle a b c) (= a b c))))

(define isocele?
(lambda (a b c)
(and (triangle? a b c) (or (= a b) (= a c) (= b c)))))

(define rectangle?
(lambda (a b c)
(and (triangle? a b c)
(or (= (* a a) (+ (* b b) (* c c)))
(= (* b b) (+ (* a a) (* c c)))
(= (* c c) (+ (* b b) (* a a)))))))




(define (numjour-let annee)
(let* ((a (modulo annee 19))
(b (modulo annee 4))
(c (modulo annee 7))
(d (modulo (+ (* 19 a) 24) 30))
(e (modulo (+ (* 2 b) (* 4 c) (* 6 d) 5) 7)))
(+ 22 d e)))
(define (traduit jour)
(if (< jour 31)
(begin (display jour) (display" mars "))
(begin (display (- jour 31)) (display " avril "))))
(define (paques annee)
(begin (traduit (numjour annee)) (display annee)))