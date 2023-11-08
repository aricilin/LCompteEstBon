;Flocon

(require (lib "turtles.ss" "graphics"))
(turtles #t)

(define (vonkoch prof a)
  (if (= prof 0)
      (draw a)
      (begin
        (vonkoch (- prof 1)(/ a 3))
        (turn 60)
        (begin
        (vonkoch (- prof 1)(/ a 3))
        (turn -120)
        (begin
        (vonkoch (- prof 1)(/ a 3))
        (turn 60)
        (vonkoch (- prof 1)(/ a 3)))))))

(define (prepa_vonkoch)
  (begin
    (clear)
    (move (/ turtle-window-size -2))))

(prepa_vonkoch)

(vonkoch 5 turtle-window-size)
