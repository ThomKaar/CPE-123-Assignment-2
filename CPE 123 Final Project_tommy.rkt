;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |CPE 123 Final Project|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define walls-width 900)
(define walls-length 500)

(define-struct paddle [posn moving direction])
(define-struct ball [posn deltax deltay angle counter])
(define-struct ws [p1 p2 ball])
;p1 and p2 are inststances of paddle
(define (ball-bounce ws)
  (cond [(>= (posn-x (ball-posn (ws-ball ws))) walls-width) (make-ws (ws-p1 ws) (ws-p2 ws) (make-ball (ball-posn (ws-ball ws)) (* -1 (ball-deltax (ws-ball ws))) (ball-deltay (ws-ball ws)) (ball-angle (ws-ball ws)) (+ 1 (ball-counter (ws-ball ws)))))] 
        [(<= (posn-x (ball-posn (ws-ball ws))) 0)           (make-ws (ws-p1 ws) (ws-p2 ws) (make-ball (ball-posn (ws-ball ws)) (* -1 (ball-deltax (ws-ball ws))) (ball-deltay (ws-ball ws)) (ball-angle (ws-ball ws)) (+ 1 (ball-counter (ws-ball ws)))))]
        [(>= (posn-y (ball-posn (ws-ball ws))) walls-length)(make-ws (ws-p1 ws) (ws-p2 ws) (make-ball (ball-posn (ws-ball ws)) (ball-deltax  (ws-ball ws)) (* -1 (ball-deltay (ws-ball ws)))(ball-angle (ws-ball ws)) (+ 1 (ball-counter (ws-ball ws)))))]
        [(<= (posn-y (ball-posn (ws-ball ws))) 0)           (make-ws (ws-p1 ws) (ws-p2 ws) (make-ball (ball-posn (ws-ball ws)) (ball-deltax  (ws-ball ws)) (* -1 (ball-deltay (ws-ball ws)))(ball-angle (ws-ball ws)) (+ 1 (ball-counter (ws-ball ws)))))]
        [else (make-ws (ws-p1 ws) (ws-p2 ws) (make-ball (ball-posn (ws-ball ws)) (ball-deltax  (ws-ball ws)) (ball-deltay (ws-ball ws)) (ball-angle (ws-ball ws)) (+ 1 (ball-counter (ws-ball ws)))))]
        ))
;lets draw the ball
(define gp (circle 12 "solid" "red"))
(define field (empty-scene walls-width walls-length))
(place-image gp 100 100 field)

(define (drawEmpty ws)
   (place-image
          gp
          (+ (posn-x (ball-posn (ws-ball ws)))  (ball-deltax (ws-ball ws)))
          (+ (posn-y (ball-posn (ws-ball ws)))  (ball-deltay (ws-ball ws)))
          field))
(define (both a b)
  b)
(define (ballMove ws)   
   (make-ws (ws-p1 ws) (ws-p2 ws) (make-ball
                        (make-posn
                         (+ (posn-x (ball-posn (ws-ball ws)))
                            (ball-deltax (ws-ball ws)))
                         (+ (posn-y (ball-posn (ws-ball ws)))
                            (ball-deltay (ws-ball ws))))
                        (ball-deltax(ws-ball ws))
                        (ball-deltay (ws-ball ws))
                        (ball-angle (ws-ball ws))
                        (+ 1 (ball-counter (ws-ball ws))))))
(define (main ws)
  (big-bang ws
            [to-draw drawEmpty]
            [on-tick ballPhysics]))

(define test-state (make-ws (make-paddle (make-posn 0 0) #true "forward")
                            (make-paddle (make-posn 0 0) #true "forward")
                            (make-ball
                              (make-posn 100 100)
                              10
                              10
                              0
                              1)))
(define (ballPhysics ws)
  (cond
    [(even? (ball-counter (ws-ball ws))) (ballMove ws)]
    [(odd? (ball-counter (ws-ball ws))) (ball-bounce ws)]))
